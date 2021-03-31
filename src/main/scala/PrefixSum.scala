import Chisel._
import chisel3.util.{log2Ceil, log2Floor}
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import scala.math.pow

class FA extends Module {
  val io = new Bundle {
    val a = UInt(INPUT, 1)
    val b = UInt(INPUT, 1)
    val cin = UInt(INPUT, 1)
    val sum = UInt(OUTPUT, 1)
    val cout = UInt(OUTPUT, 1)
  }
  val a_xor_b = io.a ^ io.b
  io.sum := a_xor_b ^ io.cin

  val a_and_b = io.a & io.b
  val b_and_cin = io.b & io.cin
  val a_and_cin = io.a & io.cin
  io.cout := a_and_b | b_and_cin | a_and_cin
}

class CSA3(val n: Int) extends Module {
  val io = new Bundle {
    val a = UInt(INPUT, n)
    val b = UInt(INPUT, n)
    val c = UInt(INPUT, n)
    val sum = UInt(OUTPUT, n)
    val cout = UInt(OUTPUT, n + 1)
  }

  val FAs = Vec.fill(n) { Module(new FA()).io }
  val Sum = Vec.fill(n) { Wire(UInt(1.W)) }
  val Carry = Vec.fill(n + 1) { Wire(UInt(1.W)) }

  Carry(0) := UInt(0)

  for (i <- 0 until n) {
    FAs(i).a := io.a(i)
    FAs(i).b := io.b(i)
    FAs(i).cin := io.c(i)
    Sum(i) := FAs(i).sum
    Carry(i + 1) := FAs(i).cout
  }

  io.sum := Reverse(Cat(Sum))
  io.cout := Reverse(Cat(Carry))
}

class CSA4(val n: Int) extends Module {
  val io = new Bundle {
    val a = UInt(INPUT, n)
    val b = UInt(INPUT, n)
    val c = UInt(INPUT, n)
    val d = UInt(INPUT, n)
    val sum = UInt(OUTPUT, n + 1)
    val cout = UInt(OUTPUT, n + 2)
  }

  val CSA3_L1 = Module(new CSA3(n)).io
  CSA3_L1.a := io.a
  CSA3_L1.b := io.b
  CSA3_L1.c := io.c

  val CSA3_L2 = Module(new CSA3(n + 1)).io
  CSA3_L2.a := io.d
  CSA3_L2.b := CSA3_L1.sum
  CSA3_L2.c := CSA3_L1.cout

  io.sum := CSA3_L2.sum
  io.cout := CSA3_L2.cout
}

class SparsePrefixSum(val n: Int) extends Module {
  val io = new Bundle {
    val in = Bits(INPUT, n)
    val out = Bits(OUTPUT, n * 7) //일단 출력할려고 *로 함. 원래는 RSA로 뽑은 + 비트 수
  }
  
  val Up_layers = log2Floor(n)
  val Down_layers = log2Floor(n) - 1
  val CSA_UP = Vec.fill(n - 1) { Module(new CSA4(1 + Up_layers)).io }
  val CSA_DOWN = Vec.fill(n - log2Floor(16)) { Module(new CSA4(1 + Up_layers + Down_layers)).io }
  val Sum = Vec.fill(n) { Wire(UInt((1 + Up_layers + Down_layers).W)) }
  val Carry = Vec.fill(n + 1) { Wire(UInt((1 + Up_layers + Down_layers).W)) }
  val Sum_lv2 = Vec.fill(n) { Wire(UInt((1 + Up_layers + Down_layers).W)) }
  val Carry_lv2 = Vec.fill(n + 1) { Wire(UInt((1 + Up_layers + Down_layers).W)) }

  // lv0
  for (i <- 0 until n / 2) {
    CSA_UP(i).a := io.in(i)
    CSA_UP(i).b := io.in(i + 1)
    CSA_UP(i).b := UInt(0)
    CSA_UP(i).b := UInt(0)
    if (i % 2 == 0) {
      Sum(2 * i + 1) := CSA_UP(i).sum
      Carry(2 * i + 1) := CSA_UP(i).cout
    }
  }

  // lv1 ~ Up_layers
  for (layer <- 1 until Up_layers) {
    val base_csa_num1 = n - n / (math.pow(2, layer - 1).toInt)  // 이전 layer의 base CSA number
    val base_csa_num2 = n - n / (math.pow(2, layer).toInt)      // 현재 layer의 base CSA number

    for (i <- 0 until n / (2 * math.pow(2, layer).toInt)) {
      val csa_num = base_csa_num2 + i                           // 현재 CSA number
      val in_csa_num1 = base_csa_num1 + 2 * i                   // input #1에 연결할 CSA number
      val in_csa_num2 = base_csa_num1 + 2 * i + 1               // input #2에 연결할 CSA number

      CSA_UP(csa_num).a := CSA_UP(in_csa_num1).sum      
      CSA_UP(csa_num).b := CSA_UP(in_csa_num1).cout
      CSA_UP(csa_num).c := CSA_UP(in_csa_num2).sum
      CSA_UP(csa_num).d := CSA_UP(in_csa_num2).sum

      if (i % 2 == 0) {
        val wire_idx = math.pow(2, layer + 1).toInt + 4 * layer * i - 1
        Sum(wire_idx) := CSA_UP(csa_num).sum
        Carry(wire_idx) := CSA_UP(csa_num).cout
      }
    }
  }

  // 0 ~ Down_layers
  for (layer <- 0 until Down_layers) {
    val base_num = 2 * math.pow(2, layer).toInt - (layer + 2) // 현재 layer의 base CSA number
    val in_wire_idx1 = n / math.pow(2, layer + 1).toInt - 1      // base CSA의 input #1에 연결할 wire idx
    val in_wire_idx2 = n / math.pow(2, layer + 1).toInt - 1 + n / math.pow(2, layer + 2).toInt //base CSA의 input #2에 연결할 wire idx
    
    // input에 연결할 wire idx = in_wire_idx + offset
    for (i <- 0 until 2 * math.pow(2, layer).toInt - 1 - (math.pow(2, layer).toInt - 1)) {
      val csa_num = base_num + i
      val offset = i * (n / math.pow(2, layer + 1).toInt)

      CSA_DOWN(csa_num).a := Sum(in_wire_idx1 + offset)
      CSA_DOWN(csa_num).b := Carry(in_wire_idx1 + offset)
      CSA_DOWN(csa_num).c := Sum(in_wire_idx2 + offset)
      CSA_DOWN(csa_num).d := Carry(in_wire_idx2 + offset)
      Sum_lv2(in_wire_idx2 + offset) := CSA_DOWN(csa_num).sum
      Carry_lv2(in_wire_idx2 + offset) := CSA_DOWN(csa_num).cout
    }
    
    for (i <-  0 until  (math.pow(2, layer).toInt - 1)) {
      val csa_num = 2 * math.pow(2, layer + 1).toInt - (layer + 3) - (math.pow(2, layer).toInt - 1) // 현재 layer 내 csa_num

      // i가 홀수면은 2번째꺼는 wire에서 받아야되고, 짝수이면은 io.in에서 받아야함
      // output을 바로 wire에 연결만 해주면은 된다
      println("layer: " + layer + " i : " + i + " csa_num : " + csa_num)
      
      //val test2 = n / math.pow(2, layer + 2).toInt + 1
      val test2 = n / math.pow(2, layer + 2).toInt
      //println("test2 : " + test2 + " i : " + i + " i-test2 : " + (i - test2) + up_layer_i)

      // CSA_DOWN(i).a := Sum_lv2(i - test2)
      // CSA_DOWN(i).b := Carry_lv2(i - test2)
      // if (i % 2 == 0){
      //   CSA_DOWN(i).c := io.in(와이어)
      //   CSA_DOWN(i).d := UInt(0)
      // } else{
      //   CSA_DOWN(i).c := Sum(와이어)
      //   CSA_DOWN(i).d := Carry(와이어)
      // }
      // Sum_lv2(와이어) := CSA_DOWN(i).sum
      // Sum_lv2(와이어) := CSA_DOWN(i).cout
    }
  }

}
object CSA4 extends App {
  (new ChiselStage)
    .execute(
      Array("-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new SparsePrefixSum(16)))
    )
}
