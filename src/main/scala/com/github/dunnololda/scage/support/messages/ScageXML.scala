package com.github.dunnololda.scage.support.messages

import com.github.dunnololda.cli.AppProperties._
import com.github.dunnololda.cli.MySimpleLogger
import collection.mutable.HashMap
import xml.XML
import org.newdawn.slick.util.ResourceLoader
import com.github.dunnololda.scage.support.ScageColor._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scage.support.parsers.FormulaParser

case class InterfaceData(interface_id:String, x:Int = -1, y:Int = -1, xinterval:Int = 0, yinterval:Int = 0, rows:Array[RowData], color:ScageColor = DEFAULT_COLOR)
case class RowData(message_id:String, x:Int = -1, y:Int = -1, placeholders_before:Int = 0, placeholders_in_row:Int = 0, color:ScageColor = DEFAULT_COLOR)
case class MessageData(message:String, x:Int = -1, y:Int = -1, color:ScageColor = DEFAULT_COLOR)

trait ScageXMLTrait {
  def lang:String
  def lang_=(new_lang:String)

  def messagesBase:String
  def messagesBase_=(new_base:String)

  def interfacesBase:String
  def interfacesBase_=(new_base:String)

  def messagesFile:String
  def interfacesFile:String

  def xml(message_id:String, parameters:Any*):String
  def xmlOrDefault(message_id:String, parameters:Any*):String
  def xmlInterface(interface_id:String, parameters:Any*):Array[MessageData]
  def xmlInterfaceStrings(interface_id:String, parameters:Any*):Array[String]
}

class ScageXML(private var _lang:String   = property("xml.lang", "en"),
               private var messages_base:String   = property("xml.strings.base", "resources/strings/" +appName+"_strings"),
               private var interfaces_base:String = property("xml.interfaces.base", "resources/interfaces/"+appName+"_interfaces")
) extends ScageXMLTrait {
  private val log = MySimpleLogger(this.getClass.getName)

  def messagesBase = messages_base
  def messagesBase_=(new_base:String) {messages_base = new_base}

  def interfacesBase = interfaces_base
  def interfacesBase_=(new_base:String) {interfaces_base = new_base}

  def lang = _lang
  def lang_=(new_lang:String) {
    _lang = new_lang
    messages_file = messages_base + "_" + _lang + ".xml"
    xml_messages = null

    interfaces_file = interfaces_base + "_" + _lang + ".xml"
    xml_interfaces = null
  }
  
  private var messages_file = messages_base + "_" + _lang + ".xml"
  def messagesFile = messages_file

  private var xml_messages:HashMap[String, String] = null
  private def xmlMessages = xml_messages match {
    case null =>
      xml_messages = try {
        log.debug("parsing xml messages from file "+messages_file)
        XML.load(ResourceLoader.getResourceAsStream(messages_file)) match {
          case <messages>{messages_list @ _*}</messages> => {
            HashMap((for {
              message @ <message>{_*}</message> <- messages_list
              message_id = (message \ "@id").text
              if message_id != ""
              message_text = (message.text).trim
            } yield {
              log.debug("added message "+message_id)
              (message_id, message_text)
            }):_*)
          }
          case _ => HashMap[String, String]()  // TODO: log messages
        }
      } catch {
        case ex:Exception => HashMap[String, String]()  // TODO: log messages
      }
      xml_messages
    case _ => xml_messages
  }

  private def mergeMessage(xml_message:String, parameters:Any*) = {
    parameters.zipWithIndex.foldLeft(xml_message)((message, param) => {
      message.replaceAll("\\$"+param._2, param._1.toString)
    })
  }

  def xml(message_id:String, parameters:Any*):String = {
    if(message_id == "") ""
    else {
      xmlMessages.get(message_id) match {
        case Some(message) => mergeMessage(message, parameters:_*)
        case None => {
          log.warn("failed to find string with id "+message_id)
          xmlMessages += (message_id -> message_id)
          message_id
        }
      }
    }
  }

  def xmlOrDefault(message_id:String, parameters:Any*):String = {
    xmlMessages.get(message_id) match {
      case Some(message) => mergeMessage(message, parameters.tail:_*)
      case None if parameters.size > 0 => {
        log.info("default value for string id "+message_id+" is "+{
          if("" == parameters.head) "empty string" else parameters.head
        })
        xmlMessages += (message_id -> parameters.head.toString)
        mergeMessage(parameters.head.toString, parameters.tail:_*)
      }
      case _ => {
        log.warn("failed to find default message for the id "+message_id)
        xmlMessages += (message_id -> message_id)
        message_id
      }
    }
  }
  
  private def placeholdersAmount(message:String) = {
    message.foldLeft(0)((sum, char) => sum + (if(char == '$') 1 else 0))
  }

  private lazy val formula_parser = new FormulaParser(  // maybe use formula_parser from ScageProperties
      constants = HashMap("window_width"  -> property("screen.width", 800),
                          "window_height" -> property("screen.height", 600))
  )

  private var interfaces_file = interfaces_base + "_" + _lang + ".xml"
  def interfacesFile = interfaces_file

  private var xml_interfaces:HashMap[String, InterfaceData] = null
  private def xmlInterfaces = xml_interfaces match {
    case null =>
      xml_interfaces = try {
        log.debug("parsing xml interfaces from file "+interfaces_file)
        XML.load(ResourceLoader.getResourceAsStream(interfaces_file)) match {
          case <interfaces>{interfaces_list @ _*}</interfaces> => {
            HashMap((for {
              interface @ <interface>{rows_list @ _*}</interface> <- interfaces_list
              interface_id = (interface \ "@id").text
              if interface_id != ""
              interface_x = try{formula_parser.calculate((interface \ "@x").text).toInt} catch {case ex:Exception => -1}
              interface_y = try{formula_parser.calculate((interface \ "@y").text).toInt} catch {case ex:Exception => -1}
              interface_xinterval = try{formula_parser.calculate((interface \ "@xinterval").text).toInt} catch {case ex:Exception => 0}
              interface_yinterval = try{formula_parser.calculate((interface \ "@yinterval").text).toInt} catch {case ex:Exception => 0}
              interface_color = fromStringOrDefault((interface \ "@color").text)
            } yield {
              var placeholders_before = 0
              var xpos = interface_x
              var ypos = interface_y
              var curcolor = interface_color
              val messages = (for {
                row @ <row>{_*}</row> <- rows_list
                message_id = (row \ "@id").text
                message = row.text
                message_x = try{formula_parser.calculate((row \ "@x").text).toInt} catch {case ex:Exception => -1}
                message_y = try{formula_parser.calculate((row \ "@y").text).toInt} catch {case ex:Exception => -1}
                placeholders_in_row = placeholdersAmount(message)
                message_color = fromStringOrDefault((row \ "@color").text)
              } yield {
                if(message != "") {
                  xmlMessages += (message_id -> message)
                  log.debug("added message "+message_id)
                }
                val to_yield_x = if(message_x != -1) message_x else xpos  // priority to coords and color in tag row
                val to_yield_y = if(message_y != -1) message_y else ypos
                if(message_color != DEFAULT_COLOR) curcolor = message_color
                val to_yield = RowData(message_id, to_yield_x, to_yield_y, placeholders_before, placeholders_in_row, curcolor)
                placeholders_before += placeholders_in_row
                if(message_x == -1) xpos += interface_xinterval
                if(message_y == -1) ypos += interface_yinterval
                to_yield
              }).toArray
              log.debug("added interface "+interface_id)
              (interface_id, InterfaceData(interface_id, interface_x, interface_y, interface_xinterval, interface_yinterval, messages, interface_color))
            }):_*)
          }
          case _ => HashMap[String, InterfaceData]()  // TODO: log messages
        }
      } catch {
        case ex:Exception => HashMap[String, InterfaceData]()  // TODO: log messages
      }
      xml_interfaces
    case _ => xml_interfaces
  }
  
  def xmlInterface(interface_id:String, parameters:Any*):Array[MessageData] = {
    xmlInterfaces.get(interface_id) match {
      case Some(InterfaceData(_, interface_x, interface_y, interface_xinterval, interface_yinterval, rows, interface_color)) => {
        (for {
          RowData(message_id, message_x, message_y, params_from, params_take, message_color) <- rows
        } yield {
          val to_yield = MessageData(xml(message_id, (parameters.drop(params_from).take(params_take)):_*), message_x, message_y, message_color)
          to_yield
        }).toArray
      }
      case None => {
        log.warn("failed to find interface with id "+interface_id)
        xmlInterfaces += (interface_id -> InterfaceData(interface_id, rows = Array(/*RowData(interface_id)*/)))
        xmlMessages += (interface_id -> interface_id)
        Array(MessageData(interface_id))
      }
    }  
  }

  def xmlInterfaceStrings(interface_id:String, parameters:Any*):Array[String] = {
    xmlInterfaces.get(interface_id) match {
      case Some(InterfaceData(_, _, _, _, _, rows, _)) => {
        (for {
          RowData(message_id, _, _, params_from, params_take, _) <- rows
        } yield xml(message_id, (parameters.drop(params_from).take(params_take)):_*)).toArray
      }
      case None => {
        log.warn("failed to find interface with id "+interface_id)
        xmlInterfaces += (interface_id -> InterfaceData(interface_id, rows = Array(RowData(interface_id))))
        xmlMessages += (interface_id -> interface_id)
        Array(interface_id)
      }
    }
  }
}

object ScageXML extends ScageXML(
  _lang            = property("xml.lang", "en"),
  messages_base   = property("xml.strings.base", "resources/strings/" +stringProperty("app.name").toLowerCase+"_strings"),
  interfaces_base = property("xml.interfaces.base", "resources/interfaces/"+stringProperty("app.name").toLowerCase+"_interfaces")
)