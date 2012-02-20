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
  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor)
  def print(message:Any, x:Float, y:Float, size:Float) {print(message, x, y, size, DEFAULT_COLOR)}
  def print(message:Any, x:Float, y:Float, color:ScageColor) {print(message, x, y, max_font_size, color)}
  def print(message:Any, x:Float, y:Float) {print(message, x, y, max_font_size, currentColor)}

  def print(message:Any, coord:Vec, size:Float, color:ScageColor)
  def print(message:Any, coord:Vec, color:ScageColor) {print(message, coord, max_font_size, color)}
  def print(message:Any, coord:Vec, size:Float) {print(message, coord, size, DEFAULT_COLOR)}
  def print(message:Any, coord:Vec) {print(message, coord, max_font_size, DEFAULT_COLOR)}

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

  private lazy val font = try {
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

  def print(message:Any, x:Float, y:Float, size:Float, color:ScageColor) {
    val print_color = if(color != DEFAULT_COLOR) color.toSlickColor else currentColor.toSlickColor
    GL11.glPushMatrix()
    font.drawString(x, y, size, message.toString, print_color)
    GL11.glPopMatrix()
  }

  def print(message:Any, coord:Vec, size:Float, color:ScageColor) {
    val print_color = if(color != DEFAULT_COLOR) color.toSlickColor else currentColor.toSlickColor
    GL11.glPushMatrix()
    font.drawString(coord.x, coord.y, size, message.toString, print_color)
    GL11.glPopMatrix()
  }
}

object ScageMessage extends ScageMessage (
  fonts_base    = property("fonts.base", "resources/fonts/"),
  font_file     = property("font.file", "DroidSans.ttf"),
  max_font_size = property("font.max_size", 18),
  glyph_from    = property("font.glyph.from", 1024),
  glyph_to      = property("font.glyph.to", 1279)
)