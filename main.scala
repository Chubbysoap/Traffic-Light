package myproject

import chisel3._
//import chiseltest._
//import org.scalatest.flatspec.AnyFlatSpec

// Define the module to test LightLogic
class LogicModule extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(2.W))
    val cntIn = Input(UInt(4.W))
    val rst = Input(Bool())
    val dly = Input(UInt(4.W))
    val cntOut = Output(UInt(4.W))
    val nextState = Output(UInt(2.W))
  })
  val inter = Wire(UInt(2.W))

  when(io.rst){
    io.cntOut := 0.U
    io.nextState := 0.U
    inter := 0.U

  }otherwise{
    inter := Logic.LightLogic(state = io.state, cnt = io.cntIn, dly = io.dly)

    when(inter =/= io.state) {
      io.cntOut := 0.U
    } otherwise {
      io.cntOut := io.cntIn
    }

    io.nextState := inter
  }
}

class Upd8Module extends Module {
  val io = IO(new Bundle {
    val ns = Input(UInt(2.W))
    val cntIn = Input(UInt(4.W))
    val reset = Input(Bool())
    val cntOut = Output(UInt(4.W))
    val cs = Output(UInt(2.W))
  })
  val state = Reg(UInt(2.W))
  val cnt = Reg(UInt(4.W))

  when( io.reset ) {
    io.cs := 0.U
    io.cntOut := 0.U
  }otherwise{
    state := io.ns
    cnt := io.cntIn + 1.U // add 2 register so wait for clk

    io.cntOut := cnt // update to new on clock

    io.cs := state

  }
}

class Light extends Module {
  val io = IO(new Bundle {
    val rst = Input(Bool())
    val slvstate = Input(UInt(2.W)) // state of perpendicular light
    val delay = Input(UInt(4.W))
    val state = Output(UInt(2.W)) // Expose state for testing
    val cnt = Output(UInt(4.W))   // Expose counter for testing
  })

  //initalize
  val upd8 = Module(new Upd8Module())
  val log = Module(new LogicModule())


  //connect states
  log.io.state := upd8.io.cs
  upd8.io.ns := log.io.nextState
  //connect counts
  log.io.cntIn := upd8.io.cntOut
  upd8.io.cntIn := log.io.cntOut
  // connect delay
  log.io.dly := io.delay
  //reset
  upd8.io.reset := io.rst
  log.io.rst := io.rst

  //for test
  io.state := upd8.io.cs
  io.cnt := upd8.io.cntOut

}

class perpLight extends Module {    // 'slave' light  gets its state from  'master' light
  val io = IO( new Bundle() {
    val mstate = Input(UInt(4.W))
    val cnt = Input(UInt(4.W))
    val dly = Input(UInt(4.W))
    val slvstate  = Output(UInt(4.W))
  })

  when(io.mstate > 0.U){
    io.slvstate := 0.U

  }otherwise{
    when(io.cnt <6.U ){
      io.slvstate := 1.U

    }otherwise{
      io.slvstate := 2.U
    }


  }
}