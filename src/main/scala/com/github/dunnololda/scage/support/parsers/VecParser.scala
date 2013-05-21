package com.github.dunnololda.scage.support.parsers

import util.parsing.combinator.JavaTokenParsers
import com.github.dunnololda.scage.support.{DVec, Vec}
import com.github.dunnololda.cli.MySimpleLogger

class VecParser extends JavaTokenParsers {
  private val log = MySimpleLogger(this.getClass.getName)

  private lazy val vec:Parser[Vec] =
    "["~floatingPointNumber~","~floatingPointNumber~"]" ^^ {case "["~x~","~y~"]" => new Vec(x.toFloat, y.toFloat)}

  def evaluate(vec_str:String) = parseAll(vec, vec_str) match {
    case Success(result, _) =>
      log.debug("successfully parsed "+result+" from string "+vec_str)
      Some(result)
    case x @ Failure(msg, _) =>
      log.error("failed to parse Vec from stirng "+vec_str)
      None
    case x @ Error(msg, _) =>
      log.error("failed to parse Vec from stirng "+vec_str)
      None
  }
}

class DVecParser extends JavaTokenParsers {
  private val log = MySimpleLogger(this.getClass.getName)

  private lazy val dvec:Parser[DVec] =
    "["~floatingPointNumber~","~floatingPointNumber~"]" ^^ {case "["~x~","~y~"]" => new DVec(x.toDouble, y.toDouble)}

  def evaluate(vec_str:String) = parseAll(dvec, vec_str) match {
    case Success(result, _) =>
      log.debug("successfully parsed "+result+" from string "+vec_str)
      Some(result)
    case x @ Failure(msg, _) =>
      log.error("failed to parse Vec from stirng "+vec_str)
      None
    case x @ Error(msg, _) =>
      log.error("failed to parse Vec from stirng "+vec_str)
      None
  }
}

