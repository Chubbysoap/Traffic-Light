package myproject

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

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

class LogicTest extends AnyFlatSpec with ChiselScalatestTester {
  "allIn" should "cycle through states correctly" in {
    test(new Light).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      // Reset the system
      c.io.rst.poke(1.B)
      c.clock.step(1)

      // Check initial state (Red)
      c.io.state.expect(0.U, "Should start in Red after reset")
      c.io.cnt.expect(0.U, "Counter should be 0 after reset")
      c.io.rst.poke(0.B)

      // Step through Red state (should stay Red until cnt > 6)
      for (i <- 1 to 6) {
        c.io.state.expect(0.U, s"Should stay in Red at cycle $i")
        c.io.cnt.expect(i.U, s"Counter should be $i")
        c.clock.step(1)
      }

      // Next cycle: Transition to Green

      c.clock.step(1)
      c.io.state.expect(1.U, "Should transition to Green when cnt > 6")
      c.io.cnt.expect(1.U, "Counter should reset to 0")

      // Step through Green state (cnt > 6 to Yellow)
      for (i <- 1 to 6) {

        c.io.state.expect(1.U, s"Should stay in Green at cycle $i")
        c.io.cnt.expect(i.U, s"Counter should be $i")
        c.clock.step(1)
      }

      // Next cycle: Transition to Yellow
      c.clock.step(1)
      c.io.state.expect(2.U, "Should transition to Yellow when cnt > 6")
      c.io.cnt.expect(1.U, "Counter should reset to 0")

      // Step through Yellow state (cnt > 3 to Red)
      for (i <- 1 to 3) {
        c.io.state.expect(2.U, s"Should stay in Yellow at cycle $i")
        c.io.cnt.expect(i.U, s"Counter should be $i")
        c.clock.step(1)
      }

      // Next cycle: Transition back to Red
      c.clock.step(1)
      c.io.state.expect(0.U, "Should transition to Red when cnt > 3")
      c.io.cnt.expect(1.U, "Counter should reset to 0")
    }
  }
}