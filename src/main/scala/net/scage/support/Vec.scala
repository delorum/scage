package net.scage.support

import _root_.net.phys2d.math.{ROVector2f, Vector2f}
import parsers.VecParser

/**
 * I believe for now this glorious piece of code has all the needed to be the exact match to 'case Vec(x:Int, y:Int) {...}':
 * 1. lots of apply() methods
 * 2. one pretty cool unapply() method
 * 3. redefined equals(), hashCode() and canEquals() methods! (- that part was hard)
 */
object Vec {
  def apply(x:Float, y:Float) = new Vec(x, y)
  def apply(v:Vec) = v.copy
  def apply(v:ROVector2f) = new Vec(v.getX, v.getY)
  def apply(x:Double, y:Double) = new Vec(x.toFloat, y.toFloat)
  def apply() = new Vec(0, 0)
  
  def unapply(data:Any):Option[(Float, Float)] = data match {
    case v:Vec => Some(v.x, v.y)
    case _ => None
  }
  
  private lazy val vec_parser= new VecParser()
  def fromString(vec_str:String):Option[Vec] = vec_parser.evaluate(vec_str)
  def fromStringOrDefault(vec_str:String, default_vec:Vec = zero):Vec = vec_parser.evaluate(vec_str) match {
    case Some(v:Vec) => v
    case None => default_vec
  }
  
  lazy val zero = Vec(0, 0)
}

class Vec(val x:Float = 0, val y:Float = 0) {
  def ix = x.toInt
  def iy = y.toInt

  def this(v:Vec) = this(v.x, v.y)
  def this(v:ROVector2f) = this(v.getX, v.getY)
  def this(x:Double, y:Double) = this(x.toFloat, y.toFloat)
  def this(x:Int, y:Int) = this(x.toFloat, y.toFloat)
  def this() = this(0,0)

  def +(v:Vec) = Vec(x+v.x, y+v.y)
  def -(v:Vec) = Vec(x-v.x, y-v.y)

  def *(v:Vec) = x*v.x + y*v.y

  def *(k:Double) = Vec(x*k, y*k)
  def *(k:Float) = Vec(x*k, y*k)
  def *(k:Int) = Vec(x*k, y*k)

  def **(v:Vec) = Vec(x*v.x, y*v.y)

  def /(k:Double):Float = this / k.toFloat
  def /(k:Float) = if(k == 0) Vec(x*1000, y*1000) else Vec(x/k, y/k)
  def /(k:Int):Float = this / k.toFloat

  def norma2:Float = x*x + y*y
  def norma = math.sqrt(norma2).toFloat
  def n = this/norma

  def perpendicular = Vec(-y, x)
  def p = perpendicular.n

  def dist2(v:Vec) = (x - v.x)*(x - v.x) + (y - v.y)*(y - v.y)
  def dist(v:Vec) = math.sqrt(dist2(v)).toFloat

  def notZero = x != 0 || y != 0
  def isZero = x == 0 && y == 0
  override def equals(other:Any):Boolean = other match {
    case that:Vec => (that canEqual this) && this.x == that.x && this.y == that.y
    case _ => false
  }
  override val hashCode:Int = (41*(41 + x) + y).toInt
  def canEqual(other: Any) = other.isInstanceOf[Vec]

  def deg(v:Vec) = (180/math.Pi*math.acos(n * v.n)).toFloat
  def rad(v:Vec) = (math.acos(n * v.n)).toFloat
  def rotateRad(ang:Double) = Vec((x * math.cos(ang) - y * math.sin(ang)).toFloat,
                                  (x * math.sin(ang) + y * math.cos(ang)).toFloat)
  def rotate(ang:Double) = rotateRad(ang)
  def rotateDeg(ang:Double) = rotateRad(ang/180*math.Pi)

  def ::(o:Vec) = o :: List[Vec](this)

  def copy = new Vec(x, y)

  def toPhys2dVec = new Vector2f(x, y)

  override def toString = "Vec(x="+x+", y="+y+")"
}