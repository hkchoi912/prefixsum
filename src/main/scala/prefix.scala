import chisel3._
import chisel3.util._
import chisel3.util.{log2Ceil, log2Floor}
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import scala.math.pow

/**
  * need more gate but fast
  */
class FA extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W)) 
    val b = Input(UInt(1.W))
    val cin = Input(UInt(1.W))
    val sum = Output(UInt(1.W))
    val cout = Output(UInt(1.W))
  })
  val a_xor_b = io.a ^ io.b
  io.sum := a_xor_b ^ io.cin

  val a_and_b = io.a & io.b
  val b_and_cin = io.b & io.cin
  val a_and_cin = io.a & io.cin
  io.cout := a_and_b | b_and_cin | a_and_cin
}
class CSA3(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(n.W))  
    val b = Input(UInt(n.W))
    val c = Input(UInt(n.W))
    val sum = Output(UInt(n.W))
    val cout = Output(UInt((n + 1).W))
  })
  val FAs = Array.fill(n)(Module(new FA()).io)
  val Carry = Wire(Vec(n+1, Bool()))
  val Sum = Wire(Vec(n, Bool()))

  Carry(0) := 0.U(1.W)

  for (i <- 0 until n) {
    FAs(i).a := io.a(i)
    FAs(i).b := io.b(i)
    FAs(i).cin := io.c(i)
    Sum(i) := FAs(i).sum.asBool
    Carry(i + 1) := FAs(i).cout
  }

  io.sum := Sum.asUInt
  io.cout := Carry.asUInt
}

class CSA4(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(n.W))  
    val b = Input(UInt(n.W))
    val c = Input(UInt(n.W))
    val d = Input(UInt(n.W))
    val sum = Output(UInt((n + 1).W)) 
    val cout = Output(UInt((n + 1).W))
  })
 
  val CSA3_L1 = Module(new CSA3(n)).io
  CSA3_L1.a := io.a
  CSA3_L1.b := io.b
  CSA3_L1.c := io.c

  val CSA3_L2 = Module(new CSA3(n)).io
  CSA3_L2.a := io.d
  CSA3_L2.b := CSA3_L1.sum
  CSA3_L2.c := CSA3_L1.cout(n, 0)

  io.sum := Cat(CSA3_L1.cout(n), CSA3_L2.sum)
  io.cout := CSA3_L2.cout
}

class RCA(val n:Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(n.W))
    val b = Input(UInt(n.W))
    val cin = Input(UInt(1.W))
    val sum = Output(UInt(n.W))
    val cout = Output(UInt(1.W))
  })
  val FAs = Array.fill(n)(Module(new FA()).io)
  val Sum = Wire(Vec(n, Bool()))
  val Carry = Wire(Vec(n + 1, Bool()))
  
  Carry(0) := io.cin
  
  for (i <- 0 until n) {
    FAs(i).a := io.a(i)
    FAs(i).b := io.b(i)
    FAs(i).cin := Carry(i)
    Sum(i) := FAs(i).sum.asBool
    Carry(i + 1) := FAs(i).cout
  }

  io.sum := Sum.asUInt
  io.cout := Carry(n)
}

/** PrefixSum
  * 
  * @param n: number of array entries
  * @param w: width of data
  */
class PrefixSum[T <: Data](inputType: T, outputType: T, val w: Int, val n: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(n, inputType))
    val out = Output(Vec(n, outputType))
  })
  
//  layer number
  val Up_layers = log2Floor(n)
  val Down_layers = log2Floor(n) - 1

//  CSA & RCA module
  val CSA_UP = Array.fill(n - 1)(Module(new CSA4(w)).io)
  val CSA_DOWN = Array.fill(n - (1 + log2Floor(n)))(Module(new CSA4(w)).io)
  val RCA = Array.fill(n)(Module(new RCA(w)).io)

//  Sum & Carry wire
  val Sum_lv1 = Wire(Vec(n, inputType))
  val Carry_lv1 = Wire(Vec(n, inputType))
  val Sum_lv2 = Wire(Vec(n, inputType))
  val Carry_lv2 = Wire(Vec(n, inputType))

//  level 0
  for (i <- 0 until n / 2) {    
    CSA_UP(i).a := io.in(2 * i)
    CSA_UP(i).b := io.in(2 * i + 1)
    CSA_UP(i).c := 0.U(w.W)
    CSA_UP(i).d := 0.U(w.W)

    if (i % 2 == 0) {
      Sum_lv1(2 * i + 1) := CSA_UP(i).sum
      Carry_lv1(2 * i + 1) := CSA_UP(i).cout
    }
  }

  // lv1 ~ Up_layers
  for (layer <- 1 until Up_layers) {
    val base_csa_num1 = n - n / (math.pow(2, layer - 1).toInt)  // 이전 layer의 base CSA number
    val base_csa_num2 = n - n / (math.pow(2, layer).toInt)      // 현재 layer의 base CSA number
  
    for (i <- 0 until n / (2 * math.pow(2, layer).toInt)) {     // 현재 레이어의 CSA 수만큼 반복
      val csa_num = base_csa_num2 + i                           // 현재 CSA number
      val in_csa_num1 = base_csa_num1 + 2 * i                   // input #1에 연결할 CSA number
      val in_csa_num2 = base_csa_num1 + 2 * i + 1               // input #2에 연결할 CSA number
  
      CSA_UP(csa_num).a := CSA_UP(in_csa_num1).sum      
      CSA_UP(csa_num).b := CSA_UP(in_csa_num1).cout
      CSA_UP(csa_num).c := CSA_UP(in_csa_num2).sum
      CSA_UP(csa_num).d := CSA_UP(in_csa_num2).cout

      if (i % 2 == 0) {
        val wire_idx = math.pow(2, layer + 1).toInt + 4 * math.pow(2, layer - 1).toInt * i - 1
        Sum_lv1(wire_idx) := CSA_UP(csa_num).sum
        Carry_lv1(wire_idx) := CSA_UP(csa_num).cout
      }
    }
  }

//  connect input with Sum.lv1 & Carry.lv1
  for (i <- 0 until n/2) {
    Sum_lv1(i * 2) := io.in(2 * i)
    Carry_lv1(i * 2) := 0.U(w.W)
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
    
      CSA_DOWN(csa_num).a := Sum_lv1(in_wire_idx1 + offset)
      CSA_DOWN(csa_num).b := Carry_lv1(in_wire_idx1 + offset)
      CSA_DOWN(csa_num).c := Sum_lv1(in_wire_idx2 + offset)
      CSA_DOWN(csa_num).d := Carry_lv1(in_wire_idx2 + offset)
      Sum_lv2(in_wire_idx2 + offset) := CSA_DOWN(csa_num).sum
      Carry_lv2(in_wire_idx2 + offset) := CSA_DOWN(csa_num).cout
    }
    
    //  CSA_DOWN의 input이 이전 CSA_DOWN의 ouput인 case
    for (i <-  0 until  (math.pow(2, layer).toInt - 1)) {
      val csa_num = 2 * math.pow(2, layer + 1).toInt - (layer + 3) - (math.pow(2, layer).toInt - 1) + i // 현재 layer 내 csa_num
      val wire_idx = n / 2 + (n / math.pow(2, layer + 1).toInt) + (n / math.pow(2, layer + 2).toInt) + i * (n / math.pow(2, layer + 1).toInt) - 1          // 현재 csa의 wire_idx
      val offset = n / math.pow(2, layer + 2).toInt       // input #1과 input #2의 wire offset
      
      CSA_DOWN(csa_num).a := Sum_lv2(wire_idx - offset)
      CSA_DOWN(csa_num).b := Carry_lv2(wire_idx - offset)
      CSA_DOWN(csa_num).c := Sum_lv1(wire_idx)
      CSA_DOWN(csa_num).d := Carry_lv1(wire_idx)

      Sum_lv2(wire_idx) := CSA_DOWN(csa_num).sum
      Carry_lv2(wire_idx) := CSA_DOWN(csa_num).cout
    }
  }

  //0은 input이랑 바로 연결 & 1, 3, 7, 15, 31 ...은 Sum lv1과 바로 연결
  for(i <- 0 until n){
    if(isPow2(i + 1)){
      Sum_lv2(i) := Sum_lv1(i)
      Carry_lv2(i) := Carry_lv1(i)
    }
  }

  for (i <- 0 until n) {
    RCA(i).a := Sum_lv2(i)
    RCA(i).b := Carry_lv2(i)
    RCA(i).cin := 0.U
    io.out(i) := Cat(RCA(i).cout, RCA(i).sum)
  }
}

object prefixsum extends App {
  (new ChiselStage)
    .execute(
      Array("-X", "verilog"),
      Seq(ChiselGeneratorAnnotation(() => new PrefixSum(UInt(32.W), UInt(32.W), 32, 64)))
    )
}
