package net.scage

import handlers.RendererLib
import support._
import support.messages._

trait ScageLib extends ScagePropertiesTrait with ScageMessageTrait with ScageXMLTrait with RendererLib with LWJGLKeyboard with ScageColorTrait with ScageIdTrait {
  def property[A : Manifest](key:String, default: => A):A = ScageProperties.property(key, default)
  def property[A : Manifest](key:String, default: => A, condition:(A => (Boolean,  String))):A = ScageProperties.property(key, default, condition)
  
  lazy val max_font_size = ScageMessage.max_font_size
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String) {ScageMessage.print(message, x, y, size, color, align)}
  def messageBounds(message:Any, size:Float = max_font_size):Vec = ScageMessage.messageBounds(message, size)

  def lang = ScageXML.lang
  def lang_=(new_lang:String) {ScageXML.lang = new_lang}

  def messagesFile:String = ScageXML.messagesFile
  def interfacesFile:String = ScageXML.interfacesFile

  def xml(message_id:String, parameters:Any*):String = ScageXML.xml(message_id, parameters:_*)
  def xmlOrDefault(message_id:String, parameters:Any*):String = ScageXML.xmlOrDefault(message_id, parameters:_*)
  def xmlInterface(interface_id:String, parameters:Any*):Array[MessageData] = ScageXML.xmlInterface(interface_id, parameters:_*)
  def xmlInterfaceStrings(interface_id:String, parameters:Any*):Array[String] = ScageXML.xmlInterfaceStrings(interface_id, parameters:_*)

  def stopApp() {Scage.stopApp()}

  def nextId = ScageId.nextId

  // implicits

  implicit def int2vecrich(i:Int) = new {
    def *(v:Vec) = v*i
    def /(v:Vec) = v/i
  }

  implicit def long2vecrich(i:Long) = new {
    def *(v:Vec) = v*i
    def /(v:Vec) = v/i
  }

  implicit def float2vecrich(f:Float) = new {
    def *(v:Vec) = v*f
    def /(v:Vec) = v/f
  }

  implicit def double2vecrich(d:Double) = new {
    def *(v:Vec) = v*d
    def /(v:Vec) = v/d
  }

  implicit def vec2dvec(v:Vec)  = v.toDVec
  implicit def dvec2vec(dv:Vec) = dv.toVec

  // support

  def msecs = System.currentTimeMillis()
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

  def coordOnArea(coord:Vec, area:List[Vec]):Boolean = {
    if (area.length < 2) false
    else {
      val a1 = coord
      val a2 = Vec(Integer.MAX_VALUE, coord.y)
      val intersections = (area.last :: area.init).zip(area).foldLeft(0) {
        case (result, (b1, b2)) => if (areLinesIntersect(a1, a2, b1, b2)) result + 1 else result
      }
      intersections % 2 != 0
    }
  }

  def coordOnArea(coord:DVec, area:List[DVec]):Boolean = {
    if (area.length < 2) false
    else {
      val a1 = coord
      val a2 = Vec(Integer.MAX_VALUE, coord.y)
      val intersections = (area.last :: area.init).zip(area).foldLeft(0) {
        case (result, (b1, b2)) => if (areLinesIntersect(a1, a2, b1, b2)) result + 1 else result
      }
      intersections % 2 != 0
    }
  }
}

object ScageLib extends ScageLib
