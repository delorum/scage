package net.scage

import handlers.RendererLib
import support._
import support.messages._

trait ScageLib extends ScagePropertiesTrait with ScageMessageTrait with ScageXMLTrait with RendererLib with LWJGLKeyboard with ScageColorTrait with ScageIdTrait {
  def property[A : Manifest](key:String, default: => A):A = ScageProperties.property(key, default)
  def property[A : Manifest](key:String, default: => A, condition:(A => (Boolean,  String))):A = ScageProperties.property(key, default, condition)
  
  val max_font_size = ScageMessage.max_font_size
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {ScageMessage.print(message, x, y, size, color)}
  def print(message:Any, coord:Vec, size:Float, color:ScageColor) {ScageMessage.print(message, coord, size, color)}
  def messageBounds(message:Any):Vec = ScageMessage.messageBounds(message)

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
}

object ScageLib extends ScageLib
