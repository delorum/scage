package com.github.dunnololda.scage

import handlers.RendererLib
import support._
import support.messages._
import com.github.dunnololda.cli.AppProperties

object ScageLib extends ScageMessageTrait with ScageXMLTrait with RendererLib with LWJGLKeyboard with ScageColorTrait with ScageIdTrait with EventsTrait {
  def property[A : Manifest](key:String, default: => A):A                                      = AppProperties.property(key, default)
  def optProperty[A : Manifest](key:String):Option[A]                                          = AppProperties.optProperty(key)
  def reqProperty[A : Manifest](key:String):A                                                  = AppProperties.reqProperty(key)
  def property[A : Manifest](key:String, default: => A, condition:(A => (Boolean,  String))):A = AppProperties.property(key, default, condition)

  def appName = AppProperties.appName
  def appVersion = AppProperties.appVersion
  
  val max_font_size = ScageMessage.max_font_size
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String) {ScageMessage.print(message, x, y, size, color, align)}
  def messageBounds(message:Any, size:Float = max_font_size):Vec                                           = ScageMessage.messageBounds(message, size)
  def areaForMessage(message:Any, coord:Vec, size:Float = max_font_size, align:String = "center"):Seq[Vec] = ScageMessage.areaForMessage(message, coord, size, align)

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

  implicit def Int2Vecrich(i:Int) = new {
    def *(v:Vec) = v*i
    def /(v:Vec) = v/i
  }

  implicit def Long2Vecrich(i:Long) = new {
    def *(v:Vec) = v*i
    def /(v:Vec) = v/i
  }

  implicit def Float2Vecrich(f:Float) = new {
    def *(v:Vec) = v*f
    def /(v:Vec) = v/f
  }

  implicit def Double2Vecrich(d:Double) = new {
    def *(v:Vec) = v*d
    def /(v:Vec) = v/d
  }

  implicit def Vec2dvec(v:Vec)  = v.toDVec
  implicit def DVec2Vec(dv:Vec) = dv.toVec

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
  type ScageScreenApp  = com.github.dunnololda.scage.ScageScreenApp
  type ScageScreen     = com.github.dunnololda.scage.ScageScreen
  type ScreenApp       = com.github.dunnololda.scage.ScreenApp
  type Screen          = com.github.dunnololda.scage.Screen
  type Vec             = com.github.dunnololda.scage.support.Vec
  type DVec            = com.github.dunnololda.scage.support.DVec
  type MultiController = com.github.dunnololda.scage.handlers.controller2.MultiController
  type ScageColor      = com.github.dunnololda.scage.support.ScageColor
  type DefaultTrace    = com.github.dunnololda.scage.support.tracer3.DefaultTrace
  type State           = com.github.dunnololda.scage.support.State
  type Trace           = com.github.dunnololda.scage.support.tracer3.Trace
  type TraceTrait      = com.github.dunnololda.scage.support.tracer3.TraceTrait
  type ScageMessage    = com.github.dunnololda.scage.support.messages.ScageMessage

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
  val State            = com.github.dunnololda.scage.support.State
  val Trace            = com.github.dunnololda.scage.support.tracer3.Trace
  val ScageColor       = com.github.dunnololda.scage.support.ScageColor
  val ScageMessage     = com.github.dunnololda.scage.support.messages.ScageMessage
  val PathFinder       = com.github.dunnololda.scage.support.PathFinder
}
