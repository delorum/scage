package com.github.dunnololda.scage.support.messages

import com.github.dunnololda.scage.handlers.RendererLib.currentColor
import com.github.dunnololda.cli.AppProperties._
import com.github.dunnololda.scage.support.messages.unicode.UnicodeFont
import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.support.{Vec, ScageColor}
import com.github.dunnololda.scage.support.ScageColor._
import org.lwjgl.opengl.GL11

trait ScageMessageTrait {
  def max_font_size:Float

  // cannot replace it with method with default arguments as its way more inconvinient for a client apps,
  // also I have an error: two overloaded methods define default arguments (x:Float, y:Float and coord:Vec)
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String)

  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {print(message, x, y, size, color, "default")}
  def print(message:Any, x:Float, y:Float, size:Float, align:String) {print(message, x, y, size, DEFAULT_COLOR, align)}
  def print(message:Any, x:Float, y:Float, color:ScageColor, align:String) {print(message, x, y, max_font_size, color, align)}
  def print(message:Any, x:Float, y:Float, align:String) {print(message, x, y, max_font_size, DEFAULT_COLOR, align)}
  def print(message:Any, x:Float, y:Float, size:Float) {print(message, x, y, size, DEFAULT_COLOR, "default")}
  def print(message:Any, x:Float, y:Float, color:ScageColor) {print(message, x, y, max_font_size, color, "default")}
  def print(message:Any, x:Float, y:Float) {print(message, x, y, max_font_size, currentColor, "default")}

  def print(message:Any, coord:Vec, size:Float, color:ScageColor, align:String) {print(message, coord.x, coord.y, size, color, align)}
  def print(message:Any, coord:Vec, size:Float, color:ScageColor) {print(message, coord.x, coord.y, size, color, "default")}
  def print(message:Any, coord:Vec, size:Float, align:String) {print(message, coord.x, coord.y, size, DEFAULT_COLOR, align)}
  def print(message:Any, coord:Vec, color:ScageColor, align:String) {print(message, coord.x, coord.y, max_font_size, color, align)}
  def print(message:Any, coord:Vec, align:String) {print(message, coord.x, coord.y, max_font_size, DEFAULT_COLOR, align)}
  def print(message:Any, coord:Vec, size:Float) {print(message, coord.x, coord.y, size, DEFAULT_COLOR, "default")}
  def print(message:Any, coord:Vec, color:ScageColor) {print(message, coord.x, coord.y, max_font_size, color, "default")}
  def print(message:Any, coord:Vec) {print(message, coord.x, coord.y, max_font_size, DEFAULT_COLOR, "default")}

  def messageBounds(message:Any, size:Float):Vec
  def areaForMessage(message:Any, coord:Vec, size:Float, align:String):Seq[Vec]

  def printCentered(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {
    print(message, x, y, size, color, align = "center")
  }
  def printCentered(message:Any, x:Float, y:Float, size:Float) {printCentered(message:Any, x, y, size, DEFAULT_COLOR)}
  def printCentered(message:Any, x:Float, y:Float, color:ScageColor) {printCentered(message, x, y, max_font_size, color)}
  def printCentered(message:Any, x:Float, y:Float) {printCentered(message, x, y, max_font_size, currentColor)}
  def printCentered(message:Any, coord:Vec, size:Float, color:ScageColor) {printCentered(message, coord.x, coord.y, size, color)}
  def printCentered(message:Any, coord:Vec, color:ScageColor) {printCentered(message, coord, max_font_size, color)}
  def printCentered(message:Any, coord:Vec, size:Float) {printCentered(message, coord, size, DEFAULT_COLOR)}
  def printCentered(message:Any, coord:Vec) {printCentered(message, coord, max_font_size, DEFAULT_COLOR)}

  def printStrings(messages:TraversableOnce[Any], x:Float, y:Float, x_interval:Float = 0, y_interval:Float = -20, color:ScageColor = DEFAULT_COLOR) {
    var x_pos = x
    var y_pos = y
    for(message <- messages) {
      print(message, x_pos, y_pos, color)
      x_pos += x_interval
      y_pos += y_interval
    }
  }

  def printInterface(messages:TraversableOnce[MessageData], parameters:Any*) {
    for(MessageData(message, message_x, message_y, message_color) <- messages) {
      print(message, message_x, message_y, color = message_color)
    }
  }

  def addGlyphs(from:Int, to:Int): Unit
  def addGlyphs(text:String): Unit
}

class ScageMessage(
  val fonts_base:String    = property("fonts.base", "resources/fonts/"),
  val font_file:String     = property("font.file", "DroidSans.ttf"),
  val max_font_size:Float  = property("font.max_size", 18),
  val glyph_from:Int       = property("font.glyph.from", 1024),
  val glyph_to:Int         = property("font.glyph.to", 1279),
  val glyph_symbols:String = property("font.glyph.symbols", "")
) extends ScageMessageTrait {
  private val log = MySimpleLogger(this.getClass.getName)

  private var _font:UnicodeFont = _
  private def font = {
    if (_font == null) {
      reloadFont()
    }
    _font
  }

  def addGlyphs(from:Int, to:Int) {
    font.addGlyphs(from ,to)
    font.loadGlyphs()
  }

  def addGlyphs(text:String) {
    font.addGlyphs(text)
    font.loadGlyphs()
  }

  def reloadFont() {
    _font = try {
      log.info("loading font "+fonts_base+font_file+"...")
      new UnicodeFont(fonts_base+font_file, max_font_size, glyph_from, glyph_to, glyph_symbols)
    } catch {
      case e:Exception =>
        log.error("failed to create font: "+e.getLocalizedMessage)
        log.error("please provide the path to some unicode ttf font")
        System.exit(1)
        null
    }
  }

  // DEFAULT_COLOR support because client programs CAN pass DEFAULT_COLOR
  // align is one of those: top-left, top-center, top-right, center-left, center, center-right, bottom-left, bottom-center, bottom-right, default
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String) {
    val print_color = if(color != DEFAULT_COLOR) color.toSlickColor else currentColor.toSlickColor
    val bounds = messageBounds(message, size)
    val num_lines = message.toString.filter(_ == '\n').length + 1
    val line_height = bounds.y/num_lines
    val (new_x, new_y) = align match {
      case "top-left" =>
        val x_offset = x
        val y_offset = y - line_height
        (x_offset, y_offset)
      case "top-center" =>
        val x_offset = x - bounds.x/2
        val y_offset = y - line_height
        (x_offset, y_offset)
      case "top-right" =>
        val x_offset = x - bounds.x
        val y_offset = y - line_height
        (x_offset, y_offset)
      case "center-left" =>
        val x_offset = x
        val y_offset = y + (bounds.y/2 - line_height)
        (x_offset, y_offset)
      case "center" =>
        val x_offset = x - bounds.x/2
        val y_offset = y - bounds.y/2 + (bounds.y - line_height)
        (x_offset, y_offset)
      case "center-right" =>
        val x_offset = x - bounds.x
        val y_offset = y - bounds.y/2 + (bounds.y - line_height)
        (x_offset, y_offset)
      case "bottom-left" =>
        val x_offset = x
        val y_offset = y + (bounds.y - line_height)
        (x_offset, y_offset)
      case "bottom-center" =>
        val x_offset = x - bounds.x/2
        val y_offset = y + (bounds.y - line_height)
        (x_offset, y_offset)
      case "bottom-right" =>
        val x_offset = x - bounds.x
        val y_offset = y + (bounds.y - line_height)
        (x_offset, y_offset)
      case "default" => (x, y)
      case a =>
        log.warn("unknow text align: "+a)
        (x, y)
    }

    GL11.glPushMatrix()
    font.drawString(new_x.toInt, new_y.toInt, size, message.toString, print_color)
    GL11.glPopMatrix()
  }

  def messageBounds(message:Any, size:Float = max_font_size) = {
    val msg_str = new ColoredString(message.toString, DEFAULT_COLOR).text
    Vec(font.getWidth(msg_str), font.getHeight(msg_str))*(size/max_font_size)
  }

  // align is one of those: top-left, top-center, top-right, center-left, center, center-right, bottom-left, bottom-center, bottom-right, default
  def areaForMessage(message:Any, coord:Vec, size:Float = max_font_size, align:String = "center"):Seq[Vec] = {
    val Vec(w, h) = messageBounds(message, size)
    align match {
      case "top-left" =>
        List(coord, coord + Vec(w, 0), coord + Vec(w, -h), coord + Vec(0, -h))
      case "top-center" =>
        List(coord + Vec(-w/2, 0), coord + Vec(w/2, 0), coord + Vec(w/2, -h), coord + Vec(-w/2, -h))
      case "top-right" =>
        List(coord + Vec(-w, 0), coord, coord + Vec(0, -h), coord + Vec(-w, -h))
      case "center-left" =>
        List(coord + Vec(0, h/2), coord + Vec(w, h/2), coord + Vec(w, -h/2), coord + Vec(0, -h/2))
      case "center" =>
        List(coord + Vec(-w/2, h/2), coord + Vec(w/2, h/2), coord + Vec(w/2, -h/2), coord + Vec(-w/2, -h/2))
      case "center-right" =>
        List(coord + Vec(-w, h/2), coord + Vec(0, h/2), coord + Vec(0, -h/2), coord + Vec(-w, -h/2))
      case "bottom-left" =>
        List(coord + Vec(0, h), coord + Vec(w, h), coord + Vec(w, 0), coord)
      case "bottom-center" =>
        List(coord + Vec(-w/2, h), coord + Vec(w/2, h), coord + Vec(w/2, 0), coord + Vec(-w/2, 0))
      case "bottom-right" =>
        List(coord + Vec(-w, h), coord + Vec(0, h), coord, coord + Vec(-w, 0))
      case "default" =>
        val num_lines = message.toString.filter(_ == '\n').length + 1
        val lh = h/num_lines
        List(coord + Vec(0, h/2-lh), coord + Vec(w, h/2-lh), coord + Vec(w, -h/2-lh), coord + Vec(0, -h/2-lh))
      case a =>
        log.warn("unknown text align: "+a)
        List(coord + Vec(-w/2, h/2), coord + Vec(w/2, h/2), coord + Vec(w/2, -h/2), coord + Vec(-w/2, -h/2))
    }
  }
}

object ScageMessage extends ScageMessage (
  fonts_base    = property("fonts.base", "resources/fonts/"),
  font_file     = property("font.file", "DroidSans.ttf"),
  max_font_size = property("font.max_size", 18),
  glyph_from    = property("font.glyph.from", 1024),
  glyph_to      = property("font.glyph.to", 1279),
  glyph_symbols = property("font.glyph.symbols", "")
)