package com.github.nnnnusui.computersystem

object BooleanGateTest extends Tester {
  def main(args: Array[String]): Unit = {
    test()
  }

  import BooleanGate._
  override def test(): Unit = {
    println("NAND ->")
    println(s"\t0 | 0 | ${nand(false, false)}")
    println(s"\t0 | 1 | ${nand(false, true )}")
    println(s"\t1 | 0 | ${nand(true , false)}")
    println(s"\t1 | 1 | ${nand(true , true )}")
  }
}
