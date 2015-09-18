package com.github.dunnololda.scage.handlers.controller2

import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.Scage
import com.github.dunnololda.scage.ScageLib.coordOnArea
import com.github.dunnololda.scage.support.Vec
import org.lwjgl.input.{Keyboard, Mouse}

import scala.collection.mutable

case class KeyPress(key_code: Int, var was_pressed: Boolean, var pressed_start_time: Long, var last_pressed_time: Long) {
  def immutable: ImmutableKeyPress = ImmutableKeyPress(key_code, was_pressed, pressed_start_time, last_pressed_time)
}

case class ImmutableKeyPress(key_code: Int, was_pressed: Boolean, pressed_start_time: Long, last_pressed_time: Long)

case class MouseButtonPress(button_code: Int, var was_pressed: Boolean, var pressed_start_time: Long, var last_pressed_time: Long) {
  def immutable: ImmutableMouseButtonPress = ImmutableMouseButtonPress(button_code, was_pressed, pressed_start_time, last_pressed_time)
}

case class ImmutableMouseButtonPress(button_code: Int, was_pressed: Boolean, pressed_start_time: Long, last_pressed_time: Long)

object ScageController {
  private val key_presses = mutable.HashMap[Int, KeyPress]()
  private val mouse_button_presses = mutable.HashMap[Int, MouseButtonPress]()
}

trait ScageController extends Scage {
  private val log = MySimpleLogger(this.getClass.getName)

  protected def mappedKeyboardKeys: scala.collection.Set[Int]

  protected def mappedMouseButtons: scala.collection.Set[Int]

  protected def innerKeyPress(key_code: Int): Option[KeyPress] = {
    ScageController.key_presses.get(key_code) match {
      case skp@Some(kp: KeyPress) => skp
      case None if mappedKeyboardKeys.contains(key_code) =>
        val kp = KeyPress(key_code, was_pressed = false, 0L, 0L)
        ScageController.key_presses += (key_code -> kp)
        Some(kp)
      case _ => None
    }
  }

  def keyPress(key_code: Int): Option[ImmutableKeyPress] = innerKeyPress(key_code).map(_.immutable)

  protected def innerMouseButtonPress(mouse_button: Int): Option[MouseButtonPress] = {
    ScageController.mouse_button_presses.get(mouse_button) match {
      case smbp@Some(mbp: MouseButtonPress) => smbp
      case None if mappedMouseButtons.contains(mouse_button) =>
        val mbp = MouseButtonPress(mouse_button, was_pressed = false, 0L, 0L)
        ScageController.mouse_button_presses += (mouse_button -> mbp)
        Some(mbp)
      case _ => None
    }
  }

  def mouseButtonPress(key_code: Int): Option[ImmutableMouseButtonPress] = innerMouseButtonPress(key_code).map(_.immutable)

  def keyPressed(key_code: Int): Boolean = {
    /*val KeyPress(_, was_pressed, _) = keyPress(key_code)
    was_pressed*/
    Keyboard.isKeyDown(key_code)
  }

  def leftMousePressed: Boolean = {
    /*val MouseButtonPress(_, was_pressed, _) = mouseButtonPress(0)
    was_pressed*/
    Mouse.isButtonDown(0)
  }

  def rightMousePressed: Boolean = {
    /*val MouseButtonPress(_, was_pressed, _) = mouseButtonPress(1)
    was_pressed*/
    Mouse.isButtonDown(1)
  }

  def mouseOnArea(area: List[Vec]): Boolean = {
    coordOnArea(mouseCoord, area)
  }

  def key(key_code: Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}): Int

  def keyIgnorePause(key_code: Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}): Int

  def keyOnPause(key_code: Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}): Int

  def anykey(onKeyDown: => Any): Int

  def anykeyIgnorePause(onKeyDown: => Any): Int

  def anykeyOnPause(onKeyDown: => Any): Int

  def mouseCoord: Vec

  def isMouseMoved: Boolean

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}): Int

  def leftMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}): Int

  def leftMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}): Int

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}): Int

  def rightMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}): Int

  def rightMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}): Int

  def mouseMotion(onMotion: Vec => Any): Int

  def mouseMotionIgnorePause(onMotion: Vec => Any): Int

  def mouseMotionOnPause(onMotion: Vec => Any): Int

  def leftMouseDrag(onDrag: Vec => Any): Int

  def leftMouseDragIgnorePause(onDrag: Vec => Any): Int

  def leftMouseDragOnPause(onDrag: Vec => Any): Int

  def rightMouseDrag(onDrag: Vec => Any): Int

  def rightMouseDragIgnorePause(onDrag: Vec => Any): Int

  def rightMouseDragOnPause(onDrag: Vec => Any): Int

  def mouseWheelUp(onWheelUp: Vec => Any): Int

  def mouseWheelUpIgnorePause(onWheelUp: Vec => Any): Int

  def mouseWheelUpOnPause(onWheelUp: Vec => Any): Int

  def mouseWheelDown(onWheelDown: Vec => Any): Int

  def mouseWheelDownIgnorePause(onWheelDown: Vec => Any): Int

  def mouseWheelDownOnPause(onWheelDown: Vec => Any): Int

  private[scage] def checkControls()

  private[scage] val control_deletion_operations = defaultContainer("control_deleters")

  def delControl(control_id: Int) = {
    _delOperation(control_id, show_warnings = true).foreach(op => op.op())
  }

  def delControls(control_ids: Int*) {
    control_ids.foreach(control_id => {
      _delOperation(control_id, show_warnings = true).foreach(op => op.op())
    })
  }

  def delAllControls() {
    control_deletion_operations.operations.map(_.op_id).foreach(control_id => {
      _delOperation(control_id, show_warnings = true).foreach(op => op.op())
    })
  }

  def delAllControlsExcept(except_control_ids: Int*) {
    control_deletion_operations.operations.map(_.op_id).filterNot(op_id => {
      except_control_ids.contains(op_id)
    }).foreach(control_id => {
      _delOperation(control_id, show_warnings = true).foreach(op => op.op())
    })
  }
}

