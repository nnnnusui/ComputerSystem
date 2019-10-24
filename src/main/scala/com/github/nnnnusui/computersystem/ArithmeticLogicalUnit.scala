package com.github.nnnnusui.computersystem

import BooleanGate._

object ArithmeticLogicalUnit {
  def halfAdder(_1: Boolean, _2: Boolean): CarryableBit
    = CarryableBit(and(_1, _2), xor(_1, _2))
  def fullAdder(_1: Boolean, _2: Boolean, _3: Boolean): CarryableBit = {
    val half1 = halfAdder(_1, _2)
    val half2 = halfAdder(half1.sum, _3)
    CarryableBit(or(half1.carry, half2.carry), half2.sum)
  }
}
case class CarryableBit(carry: Boolean, sum: Boolean)