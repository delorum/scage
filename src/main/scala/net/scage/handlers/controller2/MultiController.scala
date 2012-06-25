package net.scage.handlers.controller2

import net.scage.support.Vec
import org.lwjgl.input.{Keyboard, Mouse}
import collection.mutable.{HashMap, ArrayBuffer}
import net.scage.support.ScageId._

case class MultiKeyEvent(var was_pressed:Boolean, var last_pressed_time:Long, repeat_time: () => Long, onKeyDown: () => Any, onKeyUp: () => Any)
case class MultiMouseButtonEvent(var was_pressed:Boolean, var last_pressed_time:Long, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)

trait MultiController extends ScageController {
  private var keyboard_keys = HashMap[Int, ArrayBuffer[MultiKeyEvent]]()  // was_pressed, last_pressed_time, repeat_time, onKeyDown, onKeyUp
  private var anykeys = ArrayBuffer[() => Any]()
  private var mouse_buttons = HashMap[Int, ArrayBuffer[MultiMouseButtonEvent]]()
  private var mouse_motions = ArrayBuffer[Vec => Any]()
  private var mouse_drag_motions = HashMap[Int, ArrayBuffer[Vec => Any]]()
  private var mouse_wheel_ups = ArrayBuffer[Vec => Any]()
  private var mouse_wheel_downs = ArrayBuffer[Vec => Any]()

  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    val event = MultiKeyEvent(false, 0, () => repeat_time, () => if(!onPause) onKeyDown, () => if(!onPause) onKeyUp)
    if(keyboard_keys.contains(key_code)) keyboard_keys(key_code) += event
    else keyboard_keys(key_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => keyboard_keys(key_code) -= event)
  }
  def keyNoPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    val event = MultiKeyEvent(false, 0, () => repeat_time, () => onKeyDown, () => onKeyUp)
    if(keyboard_keys.contains(key_code)) keyboard_keys(key_code) += event
    else keyboard_keys(key_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => keyboard_keys(key_code) -= event)
  }
  def keyPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    val event = MultiKeyEvent(false, 0, () => repeat_time, () => if(onPause) onKeyDown, () => if(onPause) onKeyUp)
    if(keyboard_keys.contains(key_code)) keyboard_keys(key_code) += event
    else keyboard_keys(key_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => keyboard_keys(key_code) -= event)
  }

  def anykey(onKeyDown: => Any) = {
    val event = () => if(!onPause) onKeyDown
    anykeys += event
    deletion_operations.addOp(() => anykeys -= event)
  }
  def anykeyNoPause(onKeyDown: => Any) = {
    val event = () => onKeyDown
    anykeys += event
    deletion_operations.addOp(() => anykeys -= event)
  }

  def mouseCoord = Vec(Mouse.getX, Mouse.getY)
  def isMouseMoved = Mouse.getDX != 0 || Mouse.getDY != 0
  private def mouseButton(button_code:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) = {
    val event = MultiMouseButtonEvent(false, 0, () => repeat_time, onButtonDown, onButtonUp)
    if(mouse_buttons.contains(button_code)) mouse_buttons(button_code) += event
    else mouse_buttons(button_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => mouse_buttons(button_code) -= event)
  }

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(!onPause) onBtnDown(mouse_coord), mouse_coord => if(!onPause) onBtnUp(mouse_coord))
  }
  def leftMouseNoPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, onBtnDown, onBtnUp)
  }

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(!onPause) onBtnDown(mouse_coord), mouse_coord => if(!onPause) onBtnUp(mouse_coord))
  }
  def rightMouseNoPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, onBtnDown, onBtnUp)
  }

  def mouseMotion(onMotion: Vec => Any) = {
    val event = {mouse_coord:Vec => if(!onPause) onMotion(mouse_coord)}
    mouse_motions += event
    deletion_operations.addOp(() => mouse_motions -= event)
  }
  def mouseMotionNoPause(onMotion: Vec => Any) = {
    mouse_motions += onMotion
    deletion_operations.addOp(() => mouse_motions -= onMotion)
  }

  private def mouseDrag(button_code:Int, onDrag: Vec => Any) = {
    if(mouse_drag_motions.contains(button_code)) mouse_drag_motions(button_code) += onDrag
    else mouse_drag_motions(button_code) = ArrayBuffer(onDrag)
    deletion_operations.addOp(() => mouse_drag_motions(button_code) -= onDrag)
  }

  def leftMouseDrag(onDrag: Vec => Any) = {
    mouseDrag(0, mouse_coord => if(!onPause) onDrag(mouse_coord))
  }
  def leftMouseDragNoPause(onDrag: Vec => Any) = {
    mouseDrag(0, onDrag)
  }

  def rightMouseDrag(onDrag: Vec => Any) = {
    mouseDrag(1, mouse_coord => if(!onPause) onDrag(mouse_coord))
  }
  def rightMouseDragNoPause(onDrag: Vec => Any) = {
    mouseDrag(1, onDrag)
  }

  def mouseWheelUp(onWheelUp: Vec => Any) = {
    val event = {mouse_coord:Vec => if(!onPause) onWheelUp(mouse_coord)}
    mouse_wheel_ups += event
    deletion_operations.addOp(() => mouse_wheel_ups -= event)
  }
  def mouseWheelUpNoPause(onWheelUp: Vec => Any) = {
    mouse_wheel_ups += onWheelUp
    deletion_operations.addOp(() => mouse_wheel_ups -= onWheelUp)
  }

  def mouseWheelDown(onWheelDown: Vec => Any) = {
    val event = {mouse_coord:Vec => if(!onPause) onWheelDown(mouse_coord)}
    mouse_wheel_downs += event
    deletion_operations.addOp(() => mouse_wheel_downs -= event)
  }
  def mouseWheelDownNoPause(onWheelDown: Vec => Any) = {
    mouse_wheel_downs += onWheelDown
    deletion_operations.addOp(() => mouse_wheel_downs -= onWheelDown)
  }

  def checkControls() {
    for {
      (key, events_for_key) <- keyboard_keys
      key_press @ KeyPress(_, was_pressed, _) = keyPress(key)
    } {
      if(Keyboard.isKeyDown(key)) {
        if(!was_pressed) {
          key_press.was_pressed = true
        }
        for {
          key_data <- events_for_key
          MultiKeyEvent(_, last_pressed_time, repeat_time_func, onKeyDown, onKeyUp) = key_data
          repeat_time = repeat_time_func()
          is_repeatable = repeat_time > 0
          if !was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)
        } {
          key_data.last_pressed_time = System.currentTimeMillis()
          onKeyDown()
        }
      } else if(was_pressed) {
        key_press.was_pressed = false
        for {
          key_data <- events_for_key
          MultiKeyEvent(_, _, _, _, onKeyUp) = key_data
        } {
          onKeyUp()
        }
      }
    }

    if(Keyboard.next && Keyboard.getEventKeyState) for(anykeydown <- anykeys) anykeydown()

    val mouse_coord = mouseCoord
    val is_mouse_moved = isMouseMoved
    if(is_mouse_moved) {
      mouse_motions.foreach(onMotion => onMotion(mouse_coord))
    }

    for {
      (button, events_for_button) <- mouse_buttons
      mouse_button_press @ MouseButtonPress(_, was_pressed, _) = mouseButtonPress(button)
    } {
      if(Mouse.isButtonDown(button)) {
        if(!was_pressed) {
          mouse_button_press.was_pressed = true
        }
        for {
          button_data <- events_for_button
          MultiMouseButtonEvent(_, last_pressed_time, repeat_time_func, onButtonDown, _) = button_data
          repeat_time = repeat_time_func()
          is_repeatable = repeat_time > 0
          if !was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)
        } {
          button_data.last_pressed_time = System.currentTimeMillis()
          onButtonDown(mouse_coord)
        }
      } else if(was_pressed) {
        mouse_button_press.was_pressed = false
        for {
          button_data <- events_for_button
          MultiMouseButtonEvent(_, _, _, _, onButtonUp) = button_data
        } {
          onButtonUp(mouse_coord)
        }
      }
    }

    if(is_mouse_moved) {
      for {
        (button, drag_motions_for_button) <- mouse_drag_motions
        if Mouse.isButtonDown(button)
        onDragMotion <- drag_motions_for_button
      } onDragMotion(mouse_coord)
    }

    Mouse.getDWheel match {
      case x if(x > 0) => mouse_wheel_ups.foreach(onWheelUp => onWheelUp(mouse_coord))
      case x if(x < 0) => mouse_wheel_downs.foreach(onWheelDown => onWheelDown(mouse_coord))
      case _ =>
    }
  }
}