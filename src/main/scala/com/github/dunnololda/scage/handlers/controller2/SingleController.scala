package com.github.dunnololda.scage.handlers.controller2

import org.lwjgl.input.{Keyboard, Mouse}
import collection.mutable
import com.github.dunnololda.scage.support.Vec

case class SingleKeyEvent(key_code:Int, repeat_time: () => Long, onKeyDown: () => Any, onKeyUp: () => Any)
case class SingleMouseButtonEvent(button_code:Int, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)
case class WindowButtonEvent(area:List[Vec], button_code:Int, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)

trait SingleController extends ScageController {
  private val keyboard_key_events = mutable.HashMap[Int, SingleKeyEvent]()  // was_pressed, last_pressed_time, repeat_time, onKeyDown, onKeyUp
  private var anykey: () => Any = () => {}
  private val mouse_button_events = mutable.HashMap[Int, SingleMouseButtonEvent]()
  private var on_mouse_motion: Vec => Any = v => {}
  private val on_mouse_drag_motion = mutable.HashMap[Int, Vec => Any]()
  private var on_mouse_wheel_up: Vec => Any = v => {}
  private var on_mouse_wheel_down: Vec => Any = v => {}

  protected def mappedKeyboardKeys:scala.collection.Set[Int] = keyboard_key_events.keySet
  protected def mappedMouseButtons:scala.collection.Set[Int] = mouse_button_events.keySet

  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    keyboard_key_events(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => if(!on_pause) onKeyDown, () => if(!on_pause) onKeyUp)
    control_deletion_operations.addOp(() => keyboard_key_events -= key_code, 0)
  }
  def keyIgnorePause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    keyboard_key_events(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => onKeyDown, () => onKeyUp)
    control_deletion_operations.addOp(() => keyboard_key_events -= key_code, 0)
  }
  def keyOnPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int = {
    keyboard_key_events(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => if(on_pause) onKeyDown, () => if(on_pause) onKeyUp)
    control_deletion_operations.addOp(() => keyboard_key_events -= key_code, 0)
  }

  def anykey(onKeyDown: => Any) = {
    anykey = () => if(!on_pause) onKeyDown
    control_deletion_operations.addOp(() => anykey = () => {}, 0)
  }
  def anykeyIgnorePause(onKeyDown: => Any) = {
    anykey = () => onKeyDown
    control_deletion_operations.addOp(() => anykey = () => {}, 0)
  }
  def anykeyOnPause(onKeyDown: => Any) = {
    anykey = () => if(on_pause) onKeyDown
    control_deletion_operations.addOp(() => anykey = () => {}, 0)
  }

  def mouseCoord = Vec(Mouse.getX, Mouse.getY)
  def isMouseMoved = Mouse.getDX != 0 || Mouse.getDY != 0
  private def mouseButton(button_code:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) = {
    mouse_button_events(button_code) = SingleMouseButtonEvent(button_code, () => repeat_time, onButtonDown, onButtonUp)
    control_deletion_operations.addOp(() => mouse_button_events -= button_code, 0)
  }

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(!on_pause) onBtnDown(mouse_coord), mouse_coord => if(!on_pause) onBtnUp(mouse_coord))
  }
  def leftMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, onBtnDown, onBtnUp)
  }
  def leftMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(on_pause) onBtnDown(mouse_coord), mouse_coord => if(on_pause) onBtnUp(mouse_coord))
  }

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(!on_pause) onBtnDown(mouse_coord), mouse_coord => if(!on_pause) onBtnUp(mouse_coord))
  }
  def rightMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(on_pause) onBtnDown(mouse_coord), mouse_coord => if(on_pause) onBtnUp(mouse_coord))
  }
  
  def mouseMotion(onMotion: Vec => Any) = {
    on_mouse_motion = mouse_coord => if(!on_pause) onMotion(mouse_coord)
    control_deletion_operations.addOp(() => on_mouse_motion = v => {}, 0)
  }
  def mouseMotionIgnorePause(onMotion: Vec => Any) = {
    on_mouse_motion = onMotion
    control_deletion_operations.addOp(() => on_mouse_motion = v => {}, 0)
  }
  def mouseMotionOnPause(onMotion: Vec => Any) = {
    on_mouse_motion = mouse_coord => if(on_pause) onMotion(mouse_coord)
    control_deletion_operations.addOp(() => on_mouse_motion = v => {}, 0)
  }

  private def mouseDrag(button_code:Int, onDrag: Vec => Any) = {
    on_mouse_drag_motion(button_code) = onDrag
    control_deletion_operations.addOp(() => on_mouse_drag_motion -= button_code, 0)
  }

  def leftMouseDrag(onDrag: Vec => Any) = {
    mouseDrag(0, mouse_coord => if(!on_pause) onDrag(mouse_coord))
  }
  def leftMouseDragIgnorePause(onDrag: Vec => Any) = {
    mouseDrag(0, onDrag)
  }
  def leftMouseDragOnPause(onDrag: Vec => Any) = {
    mouseDrag(0, mouse_coord => if(on_pause) onDrag(mouse_coord))
  }

  def rightMouseDrag(onDrag: Vec => Any) = {
    mouseDrag(1, mouse_coord => if(!on_pause) onDrag(mouse_coord))
  }
  def rightMouseDragIgnorePause(onDrag: Vec => Any) = {
    mouseDrag(1, onDrag)
  }
  def rightMouseDragOnPause(onDrag: Vec => Any) = {
    mouseDrag(1, mouse_coord => if(on_pause) onDrag(mouse_coord))
  }

  def mouseWheelUp(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = mouse_coord => if(!on_pause) onWheelUp(mouse_coord)
    control_deletion_operations.addOp(() => on_mouse_wheel_up = v => {}, 0)
  }
  def mouseWheelUpIgnorePause(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = onWheelUp
    control_deletion_operations.addOp(() => on_mouse_wheel_up = v => {}, 0)
  }
  def mouseWheelUpOnPause(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = mouse_coord => if(on_pause) onWheelUp(mouse_coord)
    control_deletion_operations.addOp(() => on_mouse_wheel_up = v => {}, 0)
  }

  def mouseWheelDown(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = mouse_coord => if(!on_pause) onWheelDown(mouse_coord)
    control_deletion_operations.addOp(() => on_mouse_wheel_down = v => {}, 0)
  }
  def mouseWheelDownIgnorePause(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = onWheelDown
    control_deletion_operations.addOp(() => on_mouse_wheel_down = v => {}, 0)
  }
  def mouseWheelDownOnPause(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = mouse_coord => if(on_pause) onWheelDown(mouse_coord)
    control_deletion_operations.addOp(() => on_mouse_wheel_down = v => {}, 0)
  }

  def checkControls() {
    for {
      (key, key_data) <- keyboard_key_events
      SingleKeyEvent(_, repeat_time_func, onKeyDown, onKeyUp) = key_data
      key_press @ KeyPress(_, was_pressed, _, last_pressed_time) <- innerKeyPress(key)
    } {
      if(Keyboard.isKeyDown(key)) {
        val repeat_time = repeat_time_func()
        val is_repeatable = repeat_time > 0
        if(!was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)) {
          if(!key_press.was_pressed) key_press.pressed_start_time = System.currentTimeMillis()
          key_press.was_pressed = true
          key_press.updateLastPressedTime(System.currentTimeMillis())
          onKeyDown()
        }
      } else if(was_pressed) {
        key_press.was_pressed = false
        onKeyUp()
      }
    }

    if(Keyboard.next && Keyboard.getEventKeyState) anykey()

    val mouse_coord = mouseCoord
    val is_mouse_moved = isMouseMoved
    if(is_mouse_moved) on_mouse_motion(mouse_coord)

    for {
      (button, button_data) <- mouse_button_events
      SingleMouseButtonEvent(_, repeat_time_func, onButtonDown, onButtonUp) = button_data
      mouse_button_press @ MouseButtonPress(_, was_pressed, _, last_pressed_time) <- innerMouseButtonPress(button)
    } {
      if(Mouse.isButtonDown(button)) {
        val repeat_time = repeat_time_func()
        val is_repeatable = repeat_time > 0
        if(!was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)) {
          if(!mouse_button_press.was_pressed) mouse_button_press.pressed_start_time = System.currentTimeMillis()
          mouse_button_press.was_pressed = true
          mouse_button_press.updateLastPressedTime(System.currentTimeMillis())
          onButtonDown(mouse_coord)
        }
      } else if(was_pressed) {
        mouse_button_press.was_pressed = false
        onButtonUp(mouse_coord)
      }
    }

    if(is_mouse_moved) {
      for {
        (button, onDragMotion) <- on_mouse_drag_motion
        if Mouse.isButtonDown(button)
      } onDragMotion(mouse_coord)
    }

    Mouse.getDWheel match {
      case x if x > 0 => on_mouse_wheel_up(mouse_coord)
      case x if x < 0 => on_mouse_wheel_down(mouse_coord)
      case _ =>
    }
  }
}