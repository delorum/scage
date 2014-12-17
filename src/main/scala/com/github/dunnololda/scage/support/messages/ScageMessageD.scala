package com.github.dunnololda.scage.support.messages

import com.github.dunnololda.scage.handlers.RendererLibD._
import com.github.dunnololda.cli.AppProperties._
import com.github.dunnololda.scage.support.messages.unicode.UnicodeFont
import com.github.dunnololda.cli.MySimpleLogger
import com.github.dunnololda.scage.support.{DVec, ScageColor}
import com.github.dunnololda.scage.support.ScageColor._
import org.lwjgl.opengl.GL11

trait ScageMessageTraitD {
  def max_font_size:Double

  // cannot replace it with method with default arguments as its way more inconvinient for a client apps,
  // also I have an error: two overloaded methods define default arguments (x:Double, y:Double and coord:DVec)
  def print(message:Any, x:Double, y:Double, size:Double, color:ScageColor, align:String)

  def print(message:Any, x:Double, y:Double, size:Double, color:ScageColor) {print(message, x, y, size, color, "default")}
  def print(message:Any, x:Double, y:Double, size:Double, align:String) {print(message, x, y, size, DEFAULT_COLOR, align)}
  def print(message:Any, x:Double, y:Double, color:ScageColor, align:String) {print(message, x, y, max_font_size, color, align)}
  def print(message:Any, x:Double, y:Double, align:String) {print(message, x, y, max_font_size, DEFAULT_COLOR, align)}
  def print(message:Any, x:Double, y:Double, size:Double) {print(message, x, y, size, DEFAULT_COLOR, "default")}
  def print(message:Any, x:Double, y:Double, color:ScageColor) {print(message, x, y, max_font_size, color, "default")}
  def print(message:Any, x:Double, y:Double) {print(message, x, y, max_font_size, currentColor, "default")}

  def print(message:Any, coord:DVec, size:Double, color:ScageColor, align:String) {print(message, coord.x, coord.y, size, color, align)}
  def print(message:Any, coord:DVec, size:Double, color:ScageColor) {print(message, coord.x, coord.y, size, color, "default")}
  def print(message:Any, coord:DVec, size:Double, align:String) {print(message, coord.x, coord.y, size, DEFAULT_COLOR, align)}
  def print(message:Any, coord:DVec, color:ScageColor, align:String) {print(message, coord.x, coord.y, max_font_size, color, align)}
  def print(message:Any, coord:DVec, align:String) {print(message, coord.x, coord.y, max_font_size, DEFAULT_COLOR, align)}
  def print(message:Any, coord:DVec, size:Double) {print(message, coord.x, coord.y, size, DEFAULT_COLOR, "default")}
  def print(message:Any, coord:DVec, color:ScageColor) {print(message, coord.x, coord.y, max_font_size, color, "default")}
  def print(message:Any, coord:DVec) {print(message, coord.x, coord.y, max_font_size, DEFAULT_COLOR, "default")}

  def messageBounds(message:Any, size:Double):DVec
  def areaForMessage(message:Any, coord:DVec, size:Double, align:String):Seq[DVec]

  def printCentered(message:Any, x:Double, y:Double, size:Double, color:ScageColor) {
    print(message, x, y, size, color, align = "center")
  }
  def printCentered(message:Any, x:Double, y:Double, size:Double) {printCentered(message:Any, x, y, size, DEFAULT_COLOR)}
  def printCentered(message:Any, x:Double, y:Double, color:ScageColor) {printCentered(message, x, y, max_font_size, color)}
  def printCentered(message:Any, x:Double, y:Double) {printCentered(message, x, y, max_font_size, currentColor)}
  def printCentered(message:Any, coord:DVec, size:Double, color:ScageColor) {printCentered(message, coord.x, coord.y, size, color)}
  def printCentered(message:Any, coord:DVec, color:ScageColor) {printCentered(message, coord, max_font_size, color)}
  def printCentered(message:Any, coord:DVec, size:Double) {printCentered(message, coord, size, DEFAULT_COLOR)}
  def printCentered(message:Any, coord:DVec) {printCentered(message, coord, max_font_size, DEFAULT_COLOR)}

  def printStrings(messages:TraversableOnce[Any], x:Double, y:Double, x_interval:Double = 0, y_interval:Double = -20, color:ScageColor = DEFAULT_COLOR) {
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
}

class ScageMessageD(
                    val fonts_base:String    = property("fonts.base", "resources/fonts/"),
                    val font_file:String     = property("font.file", "DroidSans.ttf"),
                    val max_font_size:Double  = property("font.max_size", 18),
                    val glyph_from:Int       = property("font.glyph.from", 1024),
                    val glyph_to:Int         = property("font.glyph.to", 1279)
                    ) extends ScageMessageTraitD {
  private val log = MySimpleLogger(this.getClass.getName)

  private var _font:UnicodeFont = _
  private def font = {
    if (_font == null) {
      reloadFont()
    }
    _font
  }

  def reloadFont() {
    _font = try {
      log.debug("loading font "+fonts_base+font_file+"...")
      new UnicodeFont(fonts_base+font_file, max_font_size.toFloat, glyph_from, glyph_to)
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
  def print(message:Any, x:Double, y:Double, size:Double, color:ScageColor, align:String) {
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
        val x_offset = x - bounds.ix/2
        val y_offset = y - line_height
        (x_offset, y_offset)
      case "top-right" =>
        val x_offset = x - bounds.ix
        val y_offset = y - line_height
        (x_offset, y_offset)
      case "center-left" =>
        val x_offset = x
        val y_offset = y + (bounds.y/2 - line_height)
        (x_offset, y_offset)
      case "center" =>
        val x_offset = x - bounds.ix/2
        val y_offset = y - bounds.iy/2 + (bounds.y - line_height)
        (x_offset, y_offset)
      case "center-right" =>
        val x_offset = x - bounds.ix
        val y_offset = y - bounds.iy/2 + (bounds.y - line_height)
        (x_offset, y_offset)
      case "bottom-left" =>
        val x_offset = x
        val y_offset = y + (bounds.y - line_height)
        (x_offset, y_offset)
      case "bottom-center" =>
        val x_offset = x - bounds.ix/2
        val y_offset = y + (bounds.y - line_height)
        (x_offset, y_offset)
      case "bottom-right" =>
        val x_offset = x - bounds.ix
        val y_offset = y + (bounds.y - line_height)
        (x_offset, y_offset)
      case "default" => (x, y)
      case a =>
        log.warn("unknow text align: "+a)
        (x, y)
    }

    GL11.glPushMatrix()
    font.drawString(new_x.toInt, new_y.toInt, size.toFloat, message.toString, print_color)
    GL11.glPopMatrix()
  }

  def messageBounds(message:Any, size:Double = max_font_size):DVec = {
    val msg_str = new ColoredString(message.toString, DEFAULT_COLOR).text()
    DVec(font.getWidth(msg_str), font.getHeight(msg_str))*(size/max_font_size)
  }

  // align is one of those: top-left, top-center, top-right, center-left, center, center-right, bottom-left, bottom-center, bottom-right, default
  def areaForMessage(message:Any, coord:DVec, size:Double = max_font_size, align:String = "center"):Seq[DVec] = {
    val DVec(w, h) = messageBounds(message, size)
    align match {
      case "top-left" =>
        List(coord, coord + DVec(w, 0), coord + DVec(w, -h), coord + DVec(0, -h))
      case "top-center" =>
        List(coord + DVec(-w/2, 0), coord + DVec(w/2, 0), coord + DVec(w/2, -h), coord + DVec(-w/2, -h))
      case "top-right" =>
        List(coord + DVec(-w, 0), coord, coord + DVec(0, -h), coord + DVec(-w, -h))
      case "center-left" =>
        List(coord + DVec(0, h/2), coord + DVec(w, h/2), coord + DVec(w, -h/2), coord + DVec(0, -h/2))
      case "center" =>
        List(coord + DVec(-w/2, h/2), coord + DVec(w/2, h/2), coord + DVec(w/2, -h/2), coord + DVec(-w/2, -h/2))
      case "center-right" =>
        List(coord + DVec(-w, h/2), coord + DVec(0, h/2), coord + DVec(0, -h/2), coord + DVec(-w, -h/2))
      case "bottom-left" =>
        List(coord + DVec(0, h), coord + DVec(w, h), coord + DVec(w, 0), coord)
      case "bottom-center" =>
        List(coord + DVec(-w/2, h), coord + DVec(w/2, h), coord + DVec(w/2, 0), coord + DVec(-w/2, 0))
      case "bottom-right" =>
        List(coord + DVec(-w, h), coord + DVec(0, h), coord, coord + DVec(-w, 0))
      case "default" =>
        val num_lines = message.toString.filter(_ == '\n').length + 1
        val lh = h/num_lines
        List(coord + DVec(0, h/2-lh), coord + DVec(w, h/2-lh), coord + DVec(w, -h/2-lh), coord + DVec(0, -h/2-lh))
      case a =>
        log.warn("unknown text align: "+a)
        List(coord + DVec(-w/2, h/2), coord + DVec(w/2, h/2), coord + DVec(w/2, -h/2), coord + DVec(-w/2, -h/2))
    }
  }
}

object ScageMessageD extends ScageMessageD (
  fonts_base    = property("fonts.base", "resources/fonts/"),
  font_file     = property("font.file", "DroidSans.ttf"),
  max_font_size = property("font.max_size", 18),
  glyph_from    = property("font.glyph.from", 1024),
  glyph_to      = property("font.glyph.to", 1279)
)