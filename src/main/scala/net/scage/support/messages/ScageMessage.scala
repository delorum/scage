package net.scage.support.messages

import _root_.net.scage.handlers.RendererLib._
import _root_.net.scage.support.ScageProperties._
import net.scage.support.messages.unicode.UnicodeFont
import com.weiglewilczek.slf4s.Logger
import net.scage.support.{Vec, ScageColor}
import net.scage.support.ScageColor._
import org.lwjgl.opengl.GL11

trait ScageMessageTrait {
  def max_font_size:Float

  // cannot replace it with method with default arguments as its way more inconvinient for a client apps,
  // also I have an error: two overloaded methods define default arguments (x:Float, y:Float and coord:Vec)
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String)

  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {print(message, x, y, size, color, "none")}
  def print(message:Any, x:Float, y:Float, size:Float, align:String) {print(message, x, y, size, DEFAULT_COLOR, align)}
  def print(message:Any, x:Float, y:Float, color:ScageColor, align:String) {print(message, x, y, max_font_size, color, align)}
  def print(message:Any, x:Float, y:Float, align:String) {print(message, x, y, max_font_size, DEFAULT_COLOR, align)}
  def print(message:Any, x:Float, y:Float, size:Float) {print(message, x, y, size, DEFAULT_COLOR, "none")}
  def print(message:Any, x:Float, y:Float, color:ScageColor) {print(message, x, y, max_font_size, color, "none")}
  def print(message:Any, x:Float, y:Float) {print(message, x, y, max_font_size, currentColor, "none")}

  def print(message:Any, coord:Vec, size:Float, color:ScageColor) {print(message, coord.x, coord.y, size, color, "none")}
  def print(message:Any, coord:Vec, size:Float, align:String) {print(message, coord.x, coord.y, size, DEFAULT_COLOR, align)}
  def print(message:Any, coord:Vec, color:ScageColor, align:String) {print(message, coord.x, coord.y, max_font_size, color, align)}
  def print(message:Any, coord:Vec, align:String) {print(message, coord.x, coord.y, max_font_size, DEFAULT_COLOR, align)}
  def print(message:Any, coord:Vec, size:Float) {print(message, coord.x, coord.y, size, DEFAULT_COLOR, "none")}
  def print(message:Any, coord:Vec, color:ScageColor) {print(message, coord.x, coord.y, max_font_size, color, "none")}
  def print(message:Any, coord:Vec) {print(message, coord.x, coord.y, max_font_size, DEFAULT_COLOR, "none")}

  def messageBounds(message:Any, size:Float):Vec

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
}

class ScageMessage(
  val fonts_base:String    = property("fonts.base", "resources/fonts/"),
  val font_file:String     = property("font.file", "DroidSans.ttf"),
  val max_font_size:Float  = property("font.max_size", 18),
  val glyph_from:Int       = property("font.glyph.from", 1024),
  val glyph_to:Int         = property("font.glyph.to", 1279)
) extends ScageMessageTrait {
  private val log = Logger(this.getClass.getName)

  private var font = try {
    log.debug("loading font "+fonts_base+font_file+"...")
    new UnicodeFont(fonts_base+font_file, max_font_size, glyph_from, glyph_to)
  } catch {
    case e:Exception => {
      log.error("failed to create font: "+e.getLocalizedMessage)
      log.error("please provide the path to some unicode ttf font")
      System.exit(1)
      null
    }
  }

  def reloadFont() {
    font = try {
      log.debug("loading font "+fonts_base+font_file+"...")
      new UnicodeFont(fonts_base+font_file, max_font_size, glyph_from, glyph_to)
    } catch {
      case e:Exception => {
        log.error("failed to create font: "+e.getLocalizedMessage)
        log.error("please provide the path to some unicode ttf font")
        System.exit(1)
        null
      }
    }
  }

  // DEFAULT_COLOR support because client programs CAN pass DEFAULT_COLOR
  // align one of those: left, none (same as left), center, xcenter, right
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor, align:String) {
    val print_color = if(color != DEFAULT_COLOR) color.toSlickColor else currentColor.toSlickColor

    val (new_x, new_y) = align match {
      case "center" =>
        val bounds = messageBounds(message, size)
        val num_lines = message.toString.filter(_ == '\n').length + 1
        val x_offset = x - bounds.ix/2
        val y_offset = y - bounds.iy/2 + (bounds.y - bounds.y/num_lines)
        (x_offset, y_offset)
      case "xcenter" =>
        val bounds = messageBounds(message, size)
        val num_lines = message.toString.filter(_ == '\n').length + 1
        val x_offset = x - bounds.ix/2
        val y_offset = y + (bounds.y - bounds.y/num_lines)
        (x_offset, y_offset)
      case "right" =>
        val bounds = messageBounds(message, size)
        val num_lines = message.toString.filter(_ == '\n').length + 1
        val x_offset = x - bounds.ix
        val y_offset = y + (bounds.y - bounds.y/num_lines)
        (x_offset, y_offset)
      case _ => (x, y)
    }

    GL11.glPushMatrix()
    font.drawString(new_x.toInt, new_y.toInt, size, message.toString, print_color)
    GL11.glPopMatrix()
  }

  def messageBounds(message:Any, size:Float = max_font_size) = {
    val msg_str = new ColoredString(message.toString, DEFAULT_COLOR).text()
    Vec(font.getWidth(msg_str), font.getHeight(msg_str))*(size/max_font_size)
  }
}

object ScageMessage extends ScageMessage (
  fonts_base    = property("fonts.base", "resources/fonts/"),
  font_file     = property("font.file", "DroidSans.ttf"),
  max_font_size = property("font.max_size", 18),
  glyph_from    = property("font.glyph.from", 1024),
  glyph_to      = property("font.glyph.to", 1279)
)