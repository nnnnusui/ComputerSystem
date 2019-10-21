package com.github.nnnnusui.computersystem

import org.scalatest.{Matchers, WordSpec}

class BooleanGateTest extends WordSpec with Matchers {

  val nandTest = boolTest("nand", (name, pattern, answer)=> (
      BooleanGate.nand(pattern(0), pattern(1))
     ,s": $name(${pattern(0)}, ${pattern(1)}) should be $answer"
    ), _)
  val notTest = boolTest("not", (name, pattern, answer)=> (
     BooleanGate.not(pattern(0))
    ,s": $name(${pattern(0)}) should be $answer"
   ), _)
  val andTest = boolTest("and", (name, pattern, answer)=> (
      BooleanGate.and(pattern(0), pattern(1))
     ,s": $name(${pattern(0)}, ${pattern(1)}) should be $answer"
    ), _)
  val orTest = boolTest("or", (name, pattern, answer)=> (
    BooleanGate.or(pattern(0), pattern(1))
    ,s": $name(${pattern(0)}, ${pattern(1)}) should be $answer"
  ), _)

  nandTest(Seq(true, true, true ,false)).println()
  notTest(Seq(true, false)).println()
  andTest(Seq(false, false, false, true)).println()
  orTest(Seq(false, true, true, false)).println()




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