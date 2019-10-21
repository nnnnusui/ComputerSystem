package com.github.nnnnusui.computersystem

import com.github.nnnnusui.computersystem.BooleanGate._
import org.scalatest.{Matchers, WordSpec}

class BooleanGateTest extends WordSpec with Matchers {

  "nand()" in
    getBoolArgsPattern(2).zip(Seq(true, true, true ,false))
      .foreach{case (it, r)=>
        assert(nand(it(0), it(1)) == r, s": nand(${it(0)}, ${it(1)}) should be $r")
      }


  def getBoolArgsPattern(argumentLength: Int): Seq[Seq[Boolean]] ={
    val maxIndex = scala.math.pow(2.0, argumentLength).toInt - 1
    (0 to maxIndex).map{ index =>
      index.toBinaryString
        .reverse.padTo(argumentLength, '0').reverse.toCharArray
        .map(bit=> bit == '1')
    }
  }
}