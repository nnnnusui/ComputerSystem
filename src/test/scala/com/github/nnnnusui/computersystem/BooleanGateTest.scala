package com.github.nnnnusui.computersystem

import org.scalatest.{Matchers, WordSpec}

class BooleanGateTest extends WordSpec with Matchers {

  def bool1Test(function: (Boolean) => Boolean)
    = boolTest(_, (name, pattern, answer)=> (
        function(pattern(0))
        ,s": $name(${pattern(0)}) should be $answer"
      ), _)
  def bool2Test(function: (Boolean, Boolean) => Boolean)
    = boolTest(_, (name, pattern, answer)=> (
        function(pattern(0), pattern(1))
        ,s": $name(${pattern(0)}, ${pattern(1)}) should be $answer"
      ), _)

  val nandTest = bool2Test(BooleanGate.nand)("nand", _)
  val notTest = bool1Test(BooleanGate.not)("not", _)
  val andTest = bool2Test(BooleanGate.and)("and", _)
  val orTest  = bool2Test(BooleanGate.or)("or", _)
  val xorTest = bool2Test(BooleanGate.xor)("xor", _)

  nandTest(Seq(true, true, true ,false)).println()
  notTest(Seq(true, false)).println()
  andTest(Seq(false, false, false, true)).println()
  orTest(Seq(false, true, true, true)).println()
  xorTest(Seq(false, true, true, false)).println()

  val muxTest = boolTest("mux", (name, pattern, answer)=> (
      BooleanGate.mux(pattern(0), pattern(1), pattern(2))
      ,s": $name(${pattern(0)}, ${pattern(1)}, ${pattern(2)}) should be $answer"
    ), _)
  muxTest(Seq(false, false, false, true, true, false, true, true)).println()

  println(("mux" ::
    getBoolArgsPattern(2)
      .map { it =>
        val result = BooleanGate.dmux(it(0), it(1))
        val row = it.map(it => if (it) 1 else 0).mkString(" | ")
        s"\t$row | $result"
      }.toList
    ).mkString("\n"))



  def getBoolArgsPattern(argumentLength: Int): Seq[Seq[Boolean]] ={
    val maxIndex = scala.math.pow(2.0, argumentLength).toInt - 1
    (0 to maxIndex).map{ index =>
      index.toBinaryString
        .reverse.padTo(argumentLength, '0').reverse.toCharArray
        .map(bit=> bit == '1')
    }
  }
  def boolTest(name: String
              ,function: (String, Seq[Boolean], Boolean) => (Boolean, String)
              ,collectAnswer: Seq[Boolean]): Seq[String] ={
    val patternLength = (math.log(collectAnswer.size) / math.log(2.0))
    if ((patternLength % 1) != 0)
      throw new IllegalArgumentException(s"Illegal answer pattern: [size: ${collectAnswer.size}, values: [${collectAnswer.mkString(", ")}]]")

    name :: getBoolArgsPattern(patternLength.toInt).zip(collectAnswer)
      .map{case (pattern, answer)=>
        val (result, message) = function(name, pattern, answer)
        assert(result == answer, message)
        s"\t${pattern.map(it=> if(it) 1 else 0).mkString(" | ")} | $result"
      }.toList
  }
  implicit class Rich(val src: Seq[String]){
    def println(): Unit
      = scala.Predef.println(src.mkString("\n") +"\n")
  }
}