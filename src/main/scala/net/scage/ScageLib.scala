package net.scage

import handlers.RendererLib
import support._
import support.messages._

trait ScageLib extends ScagePropertiesTrait with ScageMessageTrait with ScageXMLTrait with RendererLib with LWJGLKeyboard with ScageColorTrait {
  def property[A : Manifest](key:String, default:A):A = ScageProperties.property(key, default)
  def property[A : Manifest](key:String, default:A, condition:(A => (Boolean,  String))):A = ScageProperties.property(key, default, condition)
  
  val max_font_size = ScageMessage.max_font_size
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {ScageMessage.print(message, x, y, size, color)}
  def print(message:Any, coord:Vec, size:Float, color:ScageColor) {ScageMessage.print(message, coord, size, color)}
  
  def xml(message_id:String, parameters:Any*):String = ScageXML.xml(message_id, parameters:_*)
  def xmlOrDefault(message_id:String, parameters:Any*):String = ScageXML.xmlOrDefault(message_id, parameters:_*)
  def xmlInterface(interface_id:String, parameters:Any*):Array[MessageData] = ScageXML.xmlInterface(interface_id, parameters:_*)
  def xmlInterfaceStrings(interface_id:String, parameters:Any*):Array[String] = ScageXML.xmlInterfaceStrings(interface_id, parameters:_*)

  def stopApp() {Scage.stopApp()}
}

object ScageLib extends ScageLib
