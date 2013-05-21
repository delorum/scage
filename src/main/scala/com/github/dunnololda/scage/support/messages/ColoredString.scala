package com.github.dunnololda.scage.support.messages

import collection.mutable.HashMap
import collection.mutable.ArrayBuffer
import collection.mutable.Stack
import collection.JavaConversions._
import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scage.support.ScageColor._
import org.newdawn.slick.Color

class ColoredString(original_text:String, default_color:ScageColor) {
  def this(original_text:String, c:Color) {this(original_text, new ScageColor(c))}

  def colorSwitches():java.util.Map[Int, ScageColor] = color_switches
  def originalText() = original_text
  def text() = new_text.mkString

  private val color_switches = HashMap[Int, ScageColor]()
  private val new_text = ArrayBuffer[Char]()
  private val previous_colors = Stack[ScageColor]()
  private var pos_offset = 0

  private var is_previous_slash = false
  private def findColorSwitches(text_arr:Array[Char], pos:Int, current_color:ScageColor) {
    if(pos < text_arr.length) {
      text_arr(pos) match {
        case '\\' =>
          if(!is_previous_slash) {
            is_previous_slash = true
            pos_offset += 1
            if(pos < text_arr.length-1) findColorSwitches(text_arr, pos+1, current_color)
          } else {
            is_previous_slash = false
            new_text += text_arr(pos)
            if(pos < text_arr.length-1) findColorSwitches(text_arr, pos+1, current_color)
          }
        case '[' if !is_previous_slash && pos < text_arr.length-1 =>
          (text_arr(pos+1) match {
            case 'r' => Some(RED)
            case 'g' => Some(GREEN)
            case 'b' => Some(BLUE)
            case 'y' => Some(YELLOW)
            case _ => None
          }) match {
            case Some(color) => {
              color_switches += (pos - pos_offset) -> color
              pos_offset += 2
              previous_colors.push(current_color)
              if(pos < text_arr.length-2) findColorSwitches(text_arr, pos+2, color)
            }
            case None => {
              new_text += text_arr(pos)
              findColorSwitches(text_arr, pos+1, current_color)
            }
          }
        case ']' if !is_previous_slash =>
          val previous_color = if(previous_colors.length > 0) previous_colors.pop() else default_color
          color_switches += (pos - pos_offset) -> previous_color
          pos_offset += 1
          if(pos < text_arr.length-1) findColorSwitches(text_arr, pos+1, previous_color)
        case _ =>
          is_previous_slash = false
          new_text += text_arr(pos)
          if(pos < text_arr.length-1) findColorSwitches(text_arr, pos+1, current_color)
      }
    }
  }
  findColorSwitches(original_text.toCharArray, 0, default_color)

  override def toString = "ColoredString("+original_text+", "+text+", "+color_switches+")"
}