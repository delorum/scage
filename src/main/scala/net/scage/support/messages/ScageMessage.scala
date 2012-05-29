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
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor)    // TODO: maybe replace all of this with one method with default arguments
  def print(message:Any, x:Float, y:Float, size:Float) {print(message, x, y, size, DEFAULT_COLOR)}
  def print(message:Any, x:Float, y:Float, color:ScageColor) {print(message, x, y, max_font_size, color)}
  def print(message:Any, x:Float, y:Float) {print(message, x, y, max_font_size, currentColor)}
  def print(message:Any, coord:Vec, size:Float, color:ScageColor) {print(message, coord.x, coord.y, size, color)}
  def print(message:Any, coord:Vec, color:ScageColor) {print(message, coord, max_font_size, color)}
  def print(message:Any, coord:Vec, size:Float) {print(message, coord, size, DEFAULT_COLOR)}
  def print(message:Any, coord:Vec) {print(message, coord, max_font_size, DEFAULT_COLOR)}

  def messageBounds(message:Any, size:Float):Vec

  def printCentered(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {
    val bounds = messageBounds(message, size)
    val num_lines = message.toString.filter(_ == '\n').length + 1
    val x_offset = x - bounds.ix/2
    val y_offset = y - bounds.iy/2 + (bounds.y - bounds.y/num_lines)
    print(message, x_offset, y_offset, size, color)
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
      print(message, message_x, message_y, message_color)
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

  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {
    val print_color = if(color != DEFAULT_COLOR) color.toSlickColor else currentColor.toSlickColor
    GL11.glPushMatrix()
    font.drawString(x.toInt, y.toInt, size, message.toString, print_color)
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