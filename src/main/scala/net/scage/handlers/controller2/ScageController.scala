package net.scage.handlers.controller2

import net.scage.support.Vec
import net.scage.Scage
import collection.mutable.HashMap

case class KeyPress(key_code:Int, var was_pressed:Boolean, var last_pressed_time:Long)
case class MouseButtonPress(button_code:Int, var was_pressed:Boolean, var last_pressed_time:Long)

object ScageController {
  private val key_presses = HashMap[Int, KeyPress]()
  private val mouse_button_presses = HashMap[Int, MouseButtonPress]()
}

trait ScageController extends Scage {
  protected def keyPress(key_code:Int):KeyPress = {
    ScageController.key_presses.get(key_code) match {
      case Some(kp:KeyPress) => kp
      case None =>
        val kp = KeyPress(key_code, false, 0L)
        ScageController.key_presses += (key_code -> kp)
        kp
    }
  }

  protected def mouseButtonPress(mouse_button:Int):MouseButtonPress = {
    ScageController.mouse_button_presses.get(mouse_button) match {
      case Some(mbp:MouseButtonPress) => mbp
      case None =>
        val mbp = MouseButtonPress(mouse_button, false, 0L)
        ScageController.mouse_button_presses += (mouse_button -> mbp)
        mbp
    }
  }
  
  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {})
  def keyNoPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {})

  def anykey(onKeyDown: => Any)
  def anykeyNoPause(onKeyDown: => Any)

  def mouseCoord:Vec
  def isMouseMoved:Boolean

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {})
  def leftMouseNoPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {})

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {})
  def rightMouseNoPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {})

  def mouseMotion(onMotion: Vec => Any)
  def mouseMotionNoPause(onMotion: Vec => Any)

  def leftMouseDrag(onDrag: Vec => Any)
  def leftMouseDragNoPause(onDrag: Vec => Any)

  def rightMouseDrag(onDrag: Vec => Any)
  def rightMouseDragNoPause(onDrag: Vec => Any)

  def mouseWheelUp(onWheelUp: Vec => Any)
  def mouseWheelUpNoPause(onWheelUp: Vec => Any)

  def mouseWheelDown(onWheelDown: Vec => Any)
  def mouseWheelDownNoPause(onWheelDown: Vec => Any)

  def delKeys(key_codes_to_delete: Int*)
  def delAnyKey()
  def delAllKeys()

  def delMouseButtons(mouse_buttons_to_delete:Int*)
  def delAllMouseButtons()
  def delMouseMotion()
  def delMouseDrags(mouse_buttons_to_delete:Int*)
  def delAllMouseDrags()
  def delMouseWheelUp()
  def delMouseWheelDown()

  def delAllMouseWheelEvents() {
    delMouseWheelUp()
    delMouseWheelDown()
  }
  def delAllMouseEvents() {
    delAllMouseButtons()
    delMouseMotion()
    delAllMouseDrags()
    delAllMouseWheelEvents()
  }
  def delAllKeysAndMouseEvents() {
    delAllKeys()
    delAnyKey()
    delAllMouseEvents()
  }

  override def delAllOperations() {
    delAllKeysAndMouseEvents()
    super.delAllOperations()
  }

  def checkControls()
}

