package com.github.nnnnusui.computersystem

object BooleanGate {
  def nand(_1: Boolean, _2: Boolean): Boolean
    = !(_1 && _2)

  def not(_1: Boolean): Boolean
    = nand(_1, _1)
  def and(_1: Boolean, _2: Boolean): Boolean
    = not(nand(_1, _2))
  def or(_1: Boolean, _2: Boolean): Boolean
    = nand(nand(not(_1), _2), nand(_1, not(_2)))
}
