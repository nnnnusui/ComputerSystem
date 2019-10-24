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
  def adder(_1: Seq[Boolean], _2: Seq[Boolean]): Seq[Boolean] ={
    _1.zip(_2).foldLeft[(Boolean, List[Boolean])](false, Nil)
      {case ((beforeCarry, sumArray), current)=>
        val x = fullAdder(current._1, current._2, beforeCarry)
        (x.carry, x.sum :: sumArray)
      }._2
  }
}
case class CarryableBit(carry: Boolean, sum: Boolean)