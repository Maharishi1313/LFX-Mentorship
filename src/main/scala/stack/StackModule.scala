package stack

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage
import chisel3.experimental.ChiselEnum
import java.nio.file.Paths


// Your code starts here
class StackModule(dataWidth: Int = 16, len:Int = 1024) extends Module {
  val io = IO(new Bundle{
    val in = Input(UInt(32.W))
    val out = Output(UInt(dataWidth.W))
    val underflow = Output(UInt(1.W))
    val overflow = Output(UInt(1.W))
    val isEmpty = Output(UInt(1.W))
    val isFull = Output(UInt(1.W))
    val popped = Output(UInt(1.W))
    val peeked = Output(UInt(1.W))

  })

  // Opcodes
  val PUSH = "b0100111".U(7.W)
  val POP  = "b1000011".U(7.W)
  val PEEK = "b1000000".U(7.W)

  val opcode  = io.in(6, 0)
  val doPush  = opcode === PUSH
  val doPop   = opcode === POP
  val doPeek  = opcode === PEEK

  // Stack and stack pointer initialization
  val mem = Reg(Vec(len, UInt(dataWidth.W)))
  val sp  = RegInit(0.U(log2Ceil(len + 1).W))

  // Decoding immediate
  val imm25 = io.in(31, 7)
  val pushData = Wire(UInt(dataWidth.W))
  if (dataWidth >= 25) {
    pushData := Cat(0.U((dataWidth - 25).W), imm25)
  } else {
    pushData := imm25(dataWidth - 1, 0)
  }

  // flag registers
  val outReg        = RegInit(0.U(dataWidth.W))
  val underflowReg  = RegInit(0.U(1.W))
  val overflowReg   = RegInit(0.U(1.W))
  val poppedReg     = RegInit(0.U(1.W))
  val peekedReg     = RegInit(0.U(1.W))

  // Default values 
  underflowReg := 0.U
  overflowReg  := 0.U
  poppedReg    := 0.U
  peekedReg    := 0.U

  when (doPush) {
    when (sp === len.U) {
      // already full
      overflowReg := 1.U
    } .otherwise {
      mem(sp) := pushData
      sp := sp + 1.U
      outReg := 0.U    
    }
  } .elsewhen (doPop) {
    when (sp === 0.U) {
      // empty
      underflowReg := 1.U
      outReg := 0.U
    } .otherwise {
      val newSp = sp - 1.U
      outReg := mem(newSp)
      sp := newSp
      poppedReg := 1.U
    }
  } .elsewhen (doPeek) {
    when (sp === 0.U) {
      underflowReg := 1.U
      outReg := 0.U
    } .otherwise {
      outReg := mem(sp - 1.U)
      peekedReg := 1.U
    }
  }

  // Drive IO
  io.out        := outReg
  io.underflow  := underflowReg
  io.overflow   := overflowReg
  io.popped     := poppedReg
  io.peeked     := peekedReg
  io.isEmpty    := sp === 0.U
  io.isFull     := sp === len.U

  
}





// Your code ends here

object SVGen extends App {
  val out = Paths.get(
    "out",
    this.getClass
      .getName
      .stripSuffix("$")
  ).toString
  new ChiselStage().emitSystemVerilog(
    new StackModule(args(0).toInt, args(1).toInt),
    Array("--target-dir", out),
  )
}
