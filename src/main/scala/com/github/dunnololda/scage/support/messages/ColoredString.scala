package com.github.dunnololda.scage.support.messages

import com.github.dunnololda.scage.support.ScageColor
import com.github.dunnololda.scage.support.ScageColor._
import org.newdawn.slick.Color

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import collection.JavaConversions._

/**
 * Этот класс представляет собой специальные строки, разные участки которых движок scage будет рисовать разными цветами.
 * Другой цвет для участка задается специальной разметкой вот так: "специальная строка с [rкрасным] участком"
 * то есть, участок отделяется квадратными скобками и сразу за открывающей квадратной скобкой ставится символ нужного цвета.
 * Поддерживается вложенность: "специальная [gзел[rено-кра]сная] строка".
 * Если функционал перекрашивания не нужен, а нужно именно написать что то в квадратных скобках, то перед открывающей и закрывающей
 * скобками следует поставить слеш: "неизменяемая \[rстрока\]".
 * Поскольку оперировать одним символом не очень удобно (задано мало цветов), синтаксис разметки расширен:
 * вместо одного символа можно задавать полное имя цвета в фигурных скобках. Примеры:
 * "специальная строка с [{RED}красным] участком"
 * "специальная [{GREEN}зел[{RED}ено-кра]сная] строка"
 * @param original_text - текст с разметкой цветных участков
 * @param default_color - цвет по умолчанию, которым будут нарисованы участки вне разметки
 */
class ColoredString(original_text:String, default_color:ScageColor) {
  def this(original_text:String, c:Color) {this(original_text, new ScageColor(c))}

  private val color_switches = mutable.HashMap[Int, ScageColor]()
  private val new_text = ArrayBuffer[Char]()
  private val previous_colors = mutable.Stack[ScageColor]()
  private var pos_offset = 0

  def colorSwitches:java.util.Map[Int, ScageColor] = color_switches
  def originalText = original_text
  def text = new_text.mkString

  /**
   * метод вычисляет цвет в фигурных скобках
   * @return
   */
  private def calculateColor(text_arr:Array[Char], left_bracket_pos:Int):Option[(ScageColor, Int)] = {
    val right_bracket_pos = text_arr.indexOf('}', left_bracket_pos)
    val color_name = text_arr.drop(left_bracket_pos+1).take(right_bracket_pos-1 - left_bracket_pos).mkString
    ScageColor.fromString(color_name) match {
      case Some(color) => Some((color, right_bracket_pos - left_bracket_pos + 2))
      case None => None
    }
  }

  private var is_previous_slash = false
  private def findColorSwitches(text_arr:Array[Char], pos:Int = 0, current_color:ScageColor = default_color) {
    if(0 <= pos && pos < text_arr.length) {
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
            case 'r' => Some((RED, 2))
            case 'g' => Some((GREEN, 2))
            case 'b' => Some((BLUE, 2))
            case 'y' => Some((YELLOW, 2))
            case 'o' => Some((ORANGE, 2))
            case 'p' => Some((PURPLE, 2))
            case 'm' => Some((MAGENTA, 2))
            case 'c' => Some((CYAN, 2))
            case '{' => calculateColor(text_arr, pos+1)
            case _ => None
          }) match {
            case Some((color, offset)) =>
              color_switches += ((pos - pos_offset) -> color)
              pos_offset += offset
              previous_colors.push(current_color)
              if(pos < text_arr.length-offset) findColorSwitches(text_arr, pos+offset, color)
            case _ =>
              new_text += text_arr(pos)
              findColorSwitches(text_arr, pos+1, current_color)
          }
        case ']' if !is_previous_slash =>
          val previous_color = if(previous_colors.length > 0) previous_colors.pop() else default_color
          color_switches += ((pos - pos_offset) -> previous_color)
          pos_offset += 1
          if(pos < text_arr.length-1) findColorSwitches(text_arr, pos+1, previous_color)
        case _ =>
          is_previous_slash = false
          new_text += text_arr(pos)
          if(pos < text_arr.length-1) findColorSwitches(text_arr, pos+1, current_color)
      }
    }
  }
  findColorSwitches(original_text.toCharArray)

  override def toString = s"ColoredString($original_text, $text, $color_switches)"
}