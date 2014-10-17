package com.github.dunnololda.scage.support.parsers

import scala.math._
import scala.util.parsing.combinator._
import scala.util.Random
import com.github.dunnololda.cli.MySimpleLogger
import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Simple parser for arithmetic expressions based on Scala's Combinator Parsers framework
 * author: Peter Schmitz
 * url: http://stackoverflow.com/questions/5805496/arithmetic-expression-grammar-and-parser
 *
 * Usage example:
 * val formulaParser = new FormulaParser(
 *   constants = Map("radius" -> 8D,
 *                  "height" -> 10D,
 *                  "c" -> 299792458, // m/s
 *                  "v" -> 130 * 1000 / 60 / 60, // 130 km/h in m/s
 *                  "m" -> 80),
 *    userFcts  = Map("perimeter" -> { _.toDouble * 2 * Pi } ))
 *
 * println(formulaParser.calculate("2+3*5")) // 17.0
 * println(formulaParser.calculate("height*perimeter(radius)")) // 502.6548245743669
 * println(formulaParser.calculate("m/sqrt(1-v^2/c^2)"))  // 80.00000000003415
 */

class FormulaParser(val constants:mutable.HashMap[String,Double] = mutable.HashMap[String,Double](),
                    val userFcts: mutable.HashMap[String, String => Double] = mutable.HashMap[String, String => Double](),
                    random: Random = new Random) extends JavaTokenParsers {
  require(constants.keySet.intersect(userFcts.keySet).isEmpty)
  constants ++= Map("E" -> E, "PI" -> Pi, "Pi" -> Pi)

  private val log = MySimpleLogger(this.getClass.getName)

  private val unaryOps: Map[String,Double => Double] = Map(
    "" -> {elem:Double => elem},
    "sqrt" -> (sqrt(_)),
    "abs" -> (abs(_)),
    "floor" -> (floor(_)),
    "ceil" -> (ceil(_)),
    "ln" -> (math.log(_)),
    "round" -> (round(_)),
    "signum" -> (signum(_))
  )
  private val binaryOps1: Map[String,(Double,Double) => Double] = Map(
   "+" -> (_+_), "-" -> (_-_), "*" -> (_*_), "/" -> (_/_), "^" -> pow
  )
  private val binaryOps2: Map[String,(Double,Double) => Double] = Map(
   "max" -> max, "min" -> min
  )
  private def fold(d: Double, l: List[~[String,Double]]) = l.foldLeft(d){ case (d1,op~d2) => binaryOps1(op)(d1,d2) }
  private implicit def hashmap2Parser[V](m: mutable.HashMap[String,V]) = m.keys.map(_ ^^ identity).reduceLeft(_ | _)
  private implicit def map2Parser[V](m: Map[String,V]) = m.keys.map(_ ^^ identity).reduceLeft(_ | _)
  private def expression:  Parser[Double] = sign~term~rep(("+"|"-")~term) ^^ { case s~t~l => fold(s * t,l) }
  private def sign:        Parser[Double] = opt("+" | "-") ^^ {
    case None => 1
    case Some("+") => 1
    case Some("-") => -1
    case _ => 1}
  private def term:        Parser[Double] = longFactor~rep(("*"|"/")~longFactor) ^^ { case d~l => fold(d,l) }
  private def longFactor:  Parser[Double] = shortFactor~rep("^"~shortFactor) ^^ { case d~l => fold(d,l) }
  private def shortFactor: Parser[Double] = fpn | sign~(constant | rnd | unaryFct | binaryFct | userFct | "("~>expression<~")") ^^ { case s~x => s * x }
  private def constant:    Parser[Double] = constants ^^ (constants(_))
  private def rnd:         Parser[Double] = "rnd"~>"("~>fpn~","~fpn<~")" ^^ { case x~_~y => require(y > x); x + (y-x) * random.nextDouble } | "rnd" ^^ { _ => random.nextDouble() }
  private def fpn:         Parser[Double] = floatingPointNumber ^^ (_.toDouble)
  private def unaryFct:    Parser[Double] = unaryOps~"("~expression~")" ^^ { case op~_~d~_ => unaryOps(op)(d) }
  private def binaryFct:   Parser[Double] = binaryOps2~"("~expression~","~expression~")" ^^ { case op~_~d1~_~d2~_ => binaryOps2(op)(d1,d2) }
  private def userFct:     Parser[Double] = userFcts~"("~(expression ^^ (_.toString) | ident)<~")" ^^ { case fct~_~x => userFcts(fct)(x) }

  def calculate(formula: String) = {
    parseAll(expression, formula) match {
      case Success(result, _) =>
        log.debug("parsed formula: "+formula+" = "+result)
        result
      case x @ Failure(msg, _) => // maybe throw exceptions instead
        log.error("failed to parse formula: "+formula+"\nincorrect syntax: "+msg)
        0.0
      case x @ Error(msg, _) =>
        log.error("failed to parse formula: "+formula+"\nincorrect syntax: "+msg)
        0.0
    }
  }
}