// Logic.scala
package myproject // Optional, but recommended for organization

import chisel3._
import chisel3.util.{is, switch}

object Logic {

  val states= Map('r' -> 0.U, 'g' -> 1.U, 'y' -> 2.U)

  // Function to compute the next state (combinational logic)

  def LightLogic(state: UInt, cnt: UInt, dly :UInt): UInt = {
    // Use a register or wire for the next state
    val nextState = Wire(UInt(2.W))

    // Default assignment to avoid latches in hardware
    nextState := state

    // Switch statement for state transitions
    switch (state) {
      is(0.U) {
        when(cnt > dly+dly/2.U) {
          nextState := 1.U
        }
      }
      is(1.U) {
        when(cnt > dly) {
          nextState := 2.U
        }
      }
      is(2.U) {
        when(cnt > dly/2.U) {
          nextState := 0.U
        }
      }

    }
    nextState // Return the next state
  }


}




