package com.github.nnnnusui.computersystem

object BooleanGate {
  def nand(_1: Boolean, _2: Boolean): Boolean // an axiom
    = !(_1 && _2)

  def not(_1: Boolean): Boolean
    = nand(_1, _1)
  def and(_1: Boolean, _2: Boolean): Boolean
    = not(nand(_1, _2))
  def or(_1: Boolean, _2: Boolean): Boolean
    = nand(not(_1), not(_2))
  def xor(_1: Boolean, _2: Boolean): Boolean
    = nand(nand(not(_1), _2), nand(_1, not(_2)))

  def mux(_1: Boolean, _2: Boolean, select: Boolean): Boolean
    = and(or(_1, select), or(_2, not(select)))
  def dmux(_1: Boolean, select: Boolean): (Boolean, Boolean)
    = (and(_1, not(select)), and(_1, select))
}
