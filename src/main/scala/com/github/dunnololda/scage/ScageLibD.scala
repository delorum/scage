package com.github.dunnololda.scage

import com.github.dunnololda.cli.AppProperties
import com.github.dunnololda.scage.handlers.RendererLibD
import com.github.dunnololda.scage.support._
import com.github.dunnololda.scage.support.messages.{MessageData, _}
import net.phys2d.math.ROVector2f

import scala.language.implicitConversions

object ScageLibD extends ScageMessageTrait with ScageXMLTrait with RendererLibD with LWJGLKeyboard with ScageColorTrait with ScageIdTrait with EventsTrait {
  def property[A : Manifest](key:String, default: => A):A                                      = AppProperties.property(key, default)
  def optProperty[A : Manifest](key:String):Option[A]                                          = AppProperties.optProperty(key)
  def reqProperty[A : Manifest](key:String):A                                                  = AppProperties.reqProperty(key)
  def property[A : Manifest](key:String, default: => A, condition:(A => (Boolean,  String))):A = AppProperties.property(key, default, condition)

  def appName = AppProperties.appName
  def appVersion = AppProperties.appVersion

  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String) {ScageMessage.print(message, x, y, size, color, align)}
  def messageBounds(message:Any, size:Float = max_font_size):Vec                                            = ScageMessage.messageBounds(message, size.toFloat)
  def areaForMessage(message:Any, coord:Vec, size:Float = max_font_size, align:String = "center"):Seq[Vec] = ScageMessage.areaForMessage(message, coord.toVec, size.toFloat, align)

  def lang = ScageXML.lang
  def lang_=(new_lang:String) {ScageXML.lang = new_lang}

  def messagesBase = ScageXML.messagesBase
  def messagesBase_=(new_base:String) {ScageXML.messagesBase = new_base}

  def interfacesBase = ScageXML.interfacesBase
  def interfacesBase_=(new_base:String) {ScageXML.interfacesBase = new_base}

  def messagesFile:String   = ScageXML.messagesFile
  def interfacesFile:String = ScageXML.interfacesFile

  def xml(message_id:String, parameters:Any*):String                          = ScageXML.xml(message_id, parameters:_*)
  def xmlOrDefault(message_id:String, parameters:Any*):String                 = ScageXML.xmlOrDefault(message_id, parameters:_*)
  def xmlInterface(interface_id:String, parameters:Any*):Array[MessageData]   = ScageXML.xmlInterface(interface_id, parameters:_*)
  def xmlInterfaceStrings(interface_id:String, parameters:Any*):Array[String] = ScageXML.xmlInterfaceStrings(interface_id, parameters:_*)

  def onEventWithArguments(event_name: String)(event_action: PartialFunction[Any, Unit]):(String, Int) = Events.onEventWithArguments(event_name)(event_action)
  def onEvent(event_name: String)(event_action: => Unit):(String, Int)                                 = Events.onEvent(event_name)(event_action)
  def callEvent(event_name: String, arg: Any) {Events.callEvent(event_name, arg)}
  def callEvent(event_name: String)           {Events.callEvent(event_name)}
  def delEvents(event_ids: (String, Int)*)    {Events.delEvents(event_ids:_*)}

  def stopApp() {Scage.stopApp()}

  def nextId = ScageId.nextId

  // implicits

  implicit class Int2Vecrich(i:Int) {
    def *(v:Vec) = v*i
    def /(v:Vec) = v/i
  }

  implicit class Long2Vecrich(i:Long) {
    def *(v:Vec) = v*i
    def /(v:Vec) = v/i
  }

  implicit class Float2Vecrich(f:Float) {
    def *(v:Vec) = v * f
    def /(v:Vec) = v/f
    def toRad:Float = f/180f*math.Pi.toFloat
    def toDeg:Float = f/math.Pi.toFloat*180f
  }

  implicit class Double2Vecrich(d:Double) {
    def *(v:Vec) = v*d
    def /(v:Vec) = v/d
    def toRad:Double = d/180.0*math.Pi
    def toDeg:Double = d/math.Pi*180.0
  }

  implicit class Int2DVecrich(i:Int) {
    def *(v:DVec) = v*i
    def /(v:DVec) = v/i
  }

  implicit class Long2DVecrich(i:Long) {
    def *(v:DVec) = v*i
    def /(v:DVec) = v/i
  }

  implicit class Float2DVecrich(f:Float) {
    def *(v:DVec) = v * f
    def /(v:DVec) = v/f
  }

  implicit class Double2DVecrich(d:Double) {
    def *(v:DVec) = v*d
    def /(v:DVec) = v/d
  }

  implicit class Phys2dVec2Vec(pv:ROVector2f) {
    def toVec:Vec = Vec(pv.getX, pv.getY)
    def toDVec:DVec = DVec(pv.getX, pv.getY)
  }

  implicit def Vec2dvec(v:Vec): DVec = v.toDVec
  implicit def DVec2Vec(dv:Vec): Vec = dv.toVec

  implicit val NumericVec = new Numeric[Vec] {
    def plus(x: ScageLib.Vec, y: ScageLib.Vec): ScageLib.Vec = x + y

    def minus(x: ScageLib.Vec, y: ScageLib.Vec): ScageLib.Vec = x - y

    def times(x: ScageLib.Vec, y: ScageLib.Vec): ScageLib.Vec = ???

    def negate(x: ScageLib.Vec): ScageLib.Vec = x*(-1)

    def fromInt(x: Int): ScageLib.Vec = Vec(x, x)

    def toInt(x: ScageLib.Vec): Int = x.ix

    def toLong(x: ScageLib.Vec): Long = x.ix

    def toFloat(x: ScageLib.Vec): Float = x.x

    def toDouble(x: ScageLib.Vec): Double = x.x

    def compare(x: ScageLib.Vec, y: ScageLib.Vec): Int = ???
  }

  implicit val NumericDVec = new Numeric[DVec] {
    def plus(x: ScageLib.DVec, y: ScageLib.DVec): ScageLib.DVec = x + y

    def minus(x: ScageLib.DVec, y: ScageLib.DVec): ScageLib.DVec = x - y

    def times(x: ScageLib.DVec, y: ScageLib.DVec): ScageLib.DVec = ???

    def negate(x: ScageLib.DVec): ScageLib.DVec = x*(-1)

    def fromInt(x: Int): ScageLib.DVec = DVec(x, x)

    def toInt(x: ScageLib.DVec): Int = x.ix

    def toLong(x: ScageLib.DVec): Long = x.ix

    def toFloat(x: ScageLib.DVec): Float = x.x.toFloat

    def toDouble(x: ScageLib.DVec): Double = x.x

    def compare(x: ScageLib.DVec, y: ScageLib.DVec): Int = ???
  }

  implicit class MinOption[A](s:Seq[A]) {
    def minOption(implicit o:Ordering[A]):Option[A] = if(s.isEmpty) None else Some(s.min(o))
    def minOptionBy[B](f: A => B)(implicit o:Ordering[B]):Option[A] = if(s.isEmpty) None else Some(s.minBy(f)(o))
  }

  // support

  def msecs                  = System.currentTimeMillis()
  def msecsFrom(moment:Long) = System.currentTimeMillis() - moment

  // Vec/DVec helper methods

  def areLinesIntersect(a1: Vec, a2: Vec, b1: Vec, b2: Vec): Boolean = {
    val common = (a2.x - a1.x) * (b2.y - b1.y) - (a2.y - a1.y) * (b2.x - b1.x)
    common != 0 && {
      val rH = (a1.y - b1.y) * (b2.x - b1.x) - (a1.x - b1.x) * (b2.y - b1.y)
      val sH = (a1.y - b1.y) * (a2.x - a1.x) - (a1.x - b1.x) * (a2.y - a1.y)

      val r = rH / common
      val s = sH / common

      r >= 0 && r <= 1 && s >= 0 && s <= 1
    }
  }

  def areLinesIntersect(a1: DVec, a2: DVec, b1: DVec, b2: DVec): Boolean = {
    val common = (a2.x - a1.x) * (b2.y - b1.y) - (a2.y - a1.y) * (b2.x - b1.x)
    common != 0 && {
      val rH = (a1.y - b1.y) * (b2.x - b1.x) - (a1.x - b1.x) * (b2.y - b1.y)
      val sH = (a1.y - b1.y) * (a2.x - a1.x) - (a1.x - b1.x) * (a2.y - a1.y)

      val r = rH / common
      val s = sH / common

      r >= 0 && r <= 1 && s >= 0 && s <= 1
    }
  }

  def coordOnRect(coord:Vec, leftup:Vec, width:Float, height:Float):Boolean = {
    coord.x >= leftup.x && coord.x < leftup.x+width &&
      coord.y >= leftup.y-height && coord.y < leftup.y
  }

  def coordOnRectCentered(coord:Vec, center:Vec, width:Float, height:Float):Boolean = {
    coord.x >= (center.x-width/2) && coord.x < (center.x+width/2) &&
      coord.y >= (center.y-height/2) && coord.y < (center.y+height/2)
  }

  def coordOnArea(coord:Vec, area:Seq[Vec]):Boolean = {
    if (area.length < 2) false
    else {
      val (leftup, width, height) = {
        val (min_x, max_x, min_y, max_y) = area.foldLeft((Float.MaxValue, 0f, Float.MaxValue, 0f)) {
          case ((minx, maxx, miny, maxy), Vec(x, y)) =>
            (if(x < minx) x else minx,
              if(x > maxx) x else maxx,
              if(y < miny) y else miny,
              if(y > maxy) y else maxy)
        }
        val l = Vec(min_x, max_y)
        val w =  max_x - min_x
        val h = max_y - min_y
        (l, w, h)
      }
      if(!coordOnRect(coord, leftup, width, height)) false
      else {
        val a1 = coord
        val a2 = Vec(Integer.MAX_VALUE, coord.y)
        val intersections = (Seq(area.last) ++ area.init).zip(area).foldLeft(0) {
          case (result, (b1, b2)) => if (areLinesIntersect(a1, a2, b1, b2)) result + 1 else result
        }
        intersections % 2 != 0
      }
    }
  }

  def coordOnRect(coord:DVec, leftup:DVec, width:Double, height:Double):Boolean = {
    coord.x >= leftup.x && coord.x < leftup.x+width &&
      coord.y >= leftup.y-height && coord.y < leftup.y
  }

  def coordOnRectCentered(coord:DVec, center:DVec, width:Float, height:Float):Boolean = {
    coord.x >= (center.x-width/2) && coord.x < (center.x+width/2) &&
      coord.y >= (center.y-height/2) && coord.y < (center.y+height/2)
  }

  def coordOnArea(coord:DVec, area:Seq[DVec]):Boolean = {
    if (area.length < 2) false
    else {
      val (leftup, width, height) = {
        val (min_x, max_x, min_y, max_y) = area.foldLeft((Double.MaxValue, 0.0, Double.MaxValue, 0.0)) {
          case ((minx, maxx, miny, maxy), DVec(x, y)) =>
            (if(x < minx) x else minx,
              if(x > maxx) x else maxx,
              if(y < miny) y else miny,
              if(y > maxy) y else maxy)
        }
        val l = DVec(min_x, max_y)
        val w =  max_x - min_x
        val h = max_y - min_y
        (l, w, h)
      }
      if(!coordOnRect(coord, leftup, width, height)) false
      else {
        val a1 = coord
        val a2 = DVec(Integer.MAX_VALUE, coord.y)
        val intersections = (Seq(area.last) ++ area.init).zip(area).foldLeft(0) {
          case (result, (b1, b2)) => if (areLinesIntersect(a1, a2, b1, b2)) result + 1 else result
        }
        intersections % 2 != 0
      }
    }
  }

  // types
  type ScageApp        = com.github.dunnololda.scage.ScageApp
  type Scage           = com.github.dunnololda.scage.Scage
  type ScageScreenAppD  = com.github.dunnololda.scage.ScageScreenAppD
  type ScageScreenD     = com.github.dunnololda.scage.ScageScreenD
  type ScreenAppD       = com.github.dunnololda.scage.ScreenAppD
  type ScreenD          = com.github.dunnololda.scage.ScreenD
  type Vec             = com.github.dunnololda.scage.support.Vec
  type DVec            = com.github.dunnololda.scage.support.DVec
  type MultiController = com.github.dunnololda.scage.handlers.controller2.MultiController
  type ScageColor      = com.github.dunnololda.scage.support.ScageColor
  type DefaultTrace    = com.github.dunnololda.scage.support.tracer3.DefaultTrace
  type State           = com.github.dunnololda.state.State
  type Trace           = com.github.dunnololda.scage.support.tracer3.Trace
  type TraceTrait      = com.github.dunnololda.scage.support.tracer3.TraceTrait
  type ScageMessage    = com.github.dunnololda.scage.support.messages.ScageMessage
  type PathFinder       = com.github.dunnololda.scage.support.PathFinder

  type CoordTracer[A <: com.github.dunnololda.scage.support.tracer3.TraceTrait] = com.github.dunnololda.scage.support.tracer3.CoordTracer[A]
  type ScageTracer[A <: com.github.dunnololda.scage.support.tracer3.TraceTrait] = com.github.dunnololda.scage.support.tracer3.ScageTracer[A]

  type ScagePhysics     = com.github.dunnololda.scage.support.physics.ScagePhysics

  type Physical         = com.github.dunnololda.scage.support.physics.Physical
  type StaticLine       = com.github.dunnololda.scage.support.physics.objects.StaticLine
  type StaticPolygon    = com.github.dunnololda.scage.support.physics.objects.StaticPolygon
  type StaticBox        = com.github.dunnololda.scage.support.physics.objects.StaticBox
  type StaticBall       = com.github.dunnololda.scage.support.physics.objects.StaticBall
  type DynaBox          = com.github.dunnololda.scage.support.physics.objects.DynaBox
  type DynaBall         = com.github.dunnololda.scage.support.physics.objects.DynaBall

  val Vec              = com.github.dunnololda.scage.support.Vec
  val DVec             = com.github.dunnololda.scage.support.DVec
  val CoordTracer      = com.github.dunnololda.scage.support.tracer3.CoordTracer
  val ScageTracer      = com.github.dunnololda.scage.support.tracer3.ScageTracer
  val ScagePhysics     = com.github.dunnololda.scage.support.physics.ScagePhysics
  val State            = com.github.dunnololda.state.State
  val Trace            = com.github.dunnololda.scage.support.tracer3.Trace
  val ScageColor       = com.github.dunnololda.scage.support.ScageColor
  val ScageMessage    = com.github.dunnololda.scage.support.messages.ScageMessage
  val max_font_size    = ScageMessage.max_font_size
  val PathFinder       = com.github.dunnololda.scage.support.PathFinder
}
