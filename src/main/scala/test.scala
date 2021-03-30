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
  val Up_layers = log2Floor(n) // 4
  val Down_layers = log2Floor(n) - 1 // 3
  val CSA_UP = Vec.fill(n - 1) { Module(new CSA4(1 + Up_layers)).io }
  val CSA_DOWN = Vec.fill(n - log2Floor(16)) {
    Module(new CSA4(1 + Up_layers + Down_layers)).io
  }
  val Sum = Vec.fill(n) { Wire(UInt((1 + Up_layers + Down_layers).W)) }
  val Carry = Vec.fill(n + 1) { Wire(UInt((1 + Up_layers + Down_layers).W)) }

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
    val base_idx1 = n - n / (math.pow(2, layer - 1).toInt)
    val base_idx2 = n - n / (math.pow(2, layer).toInt)

    for (i <- 0 until n / (2 * math.pow(2, layer).toInt)) {
      CSA_UP(base_idx2 + i).a := CSA_UP(base_idx1 + 2 * i).sum        // 이거 2 * i 맞나? 아닌거 같은데???
      CSA_UP(base_idx2 + i).b := CSA_UP(base_idx1 + 2 * i).cout
      CSA_UP(base_idx2 + i).c := CSA_UP(base_idx1 + 2 * i + 1).sum
      CSA_UP(base_idx2 + i).d := CSA_UP(base_idx1 + 2 * i + 1).sum

      if (i % 2 == 0) {
        val test = math.pow(2, layer + 1).toInt + 4 * layer * i - 1
        Sum(test) := CSA_UP(base_idx2 + i).sum
        Carry(test) := CSA_UP(base_idx2 + i).cout
      }

    }
  }
  // 홀수 idx wire에 값 저장되어 있는 상태임

  // 0 ~ Down_layers
  for (layer <- 0 until Down_layers) {
    val base_idx = 2 * math.pow(2, layer).toInt - (layer + 2)
    
    val up_layer_idx1 = n / math.pow(2, layer + 1).toInt - 1
    val up_layer_idx2 = n / math.pow(2, layer + 1).toInt - 1 + n / math.pow(2, layer + 2).toInt
    
    println("base_idx : " + base_idx + " up_layer_idx1 : " + up_layer_idx1 + " up_layer_idx2 : " + up_layer_idx2)
    
    for (i <- 0 until 2 * math.pow(2, layer).toInt - 1 - (math.pow(2, layer).toInt - 1)) {
      val test = i * (n / math.pow(2, layer + 1).toInt)
      CSA_DOWN(base_idx + i).a := Sum(up_layer_idx1 + test)
      CSA_DOWN(base_idx + i).b := Carry(up_layer_idx1 + test)
      CSA_DOWN(base_idx + i).c := Sum(up_layer_idx2 + test)
      CSA_DOWN(base_idx + i).d := Carry(up_layer_idx2 + test)
    }

    for (i <- 2 * math.pow(2, layer).toInt - (math.pow(2, layer).toInt - 1) until 2 * math.pow(2, layer).toInt) {
      //println("layer: " + layer + " i : " + i + " base_idx : " + base_idx)
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
