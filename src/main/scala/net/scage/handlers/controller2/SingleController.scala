package net.scage.handlers.controller2

import net.scage.support.Vec
import org.lwjgl.input.{Keyboard, Mouse}
import collection.mutable
import net.scage.support.ScageId._

case class SingleKeyEvent(key_code:Int, repeat_time: () => Long, onKeyDown: () => Any, onKeyUp: () => Any)
case class SingleMouseButtonEvent(button_code:Int, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)
case class WindowButtonEvent(area:List[Vec], button_code:Int, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)

trait SingleController extends ScageController {
  private var keyboard_keys = mutable.HashMap[Int, SingleKeyEvent]()  // was_pressed, last_pressed_time, repeat_time, onKeyDown, onKeyUp
  private var anykey: () => Any = () => {}
  private var mouse_buttons = mutable.HashMap[Int, SingleMouseButtonEvent]()
  private var window_buttons = mutable.ArrayBuffer[(WindowButtonEvent, MouseButtonPress)]()
  private var window_buttons_info = mutable.HashMap[Int, MouseButtonPress]()
  private var on_mouse_motion: Vec => Any = v => {}
  private var on_mouse_drag_motion = mutable.HashMap[Int, Vec => Any]()
  private var on_mouse_wheel_up: Vec => Any = v => {}
  private var on_mouse_wheel_down: Vec => Any = v => {}

  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    keyboard_keys(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => if(!onPause) onKeyDown, () => if(!onPause) onKeyUp)
    deletion_operations.addOp(() => keyboard_keys -= key_code)
  }
  def keyIgnorePause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    keyboard_keys(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => onKeyDown, () => onKeyUp)
    deletion_operations.addOp(() => keyboard_keys -= key_code)
  }
  def keyOnPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int = {
    keyboard_keys(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => if(onPause) onKeyDown, () => if(onPause) onKeyUp)
    deletion_operations.addOp(() => keyboard_keys -= key_code)
  }

  def anykey(onKeyDown: => Any) = {
    anykey = () => if(!onPause) onKeyDown
    deletion_operations.addOp(() => anykey = () => {})
  }
  def anykeyIgnorePause(onKeyDown: => Any) = {
    anykey = () => onKeyDown
    deletion_operations.addOp(() => anykey = () => {})
  }
  def anykeyOnPause(onKeyDown: => Any) = {
    anykey = () => if(onPause) onKeyDown
    deletion_operations.addOp(() => anykey = () => {})
  }

  def mouseCoord = Vec(Mouse.getX, Mouse.getY)
  def isMouseMoved = Mouse.getDX != 0 || Mouse.getDY != 0
  private def mouseButton(button_code:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) = {
    mouse_buttons(button_code) = SingleMouseButtonEvent(button_code, () => repeat_time, onButtonDown, onButtonUp)
    deletion_operations.addOp(() => mouse_buttons -= button_code)
  }

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(!onPause) onBtnDown(mouse_coord), mouse_coord => if(!onPause) onBtnUp(mouse_coord))
  }
  def leftMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, onBtnDown, onBtnUp)
  }
  def leftMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(onPause) onBtnDown(mouse_coord), mouse_coord => if(onPause) onBtnUp(mouse_coord))
  }

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(!onPause) onBtnDown(mouse_coord), mouse_coord => if(!onPause) onBtnUp(mouse_coord))
  }
  def rightMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(onPause) onBtnDown(mouse_coord), mouse_coord => if(onPause) onBtnUp(mouse_coord))
  }

  private def windowButton(area:List[Vec], button:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) = {
    val window_button_id = nextId
    val press = MouseButtonPress(button, was_pressed = false, 0L)
    val window_button_data = (WindowButtonEvent(area, button, () => repeat_time, onButtonDown, onButtonUp), press)
    window_buttons += window_button_data
    window_buttons_info += (window_button_id -> press)
    deletion_operations.addOp(window_button_id, () => {window_buttons -= window_button_data; window_buttons_info -= window_button_id})
  }
  def windowButtonInfo(window_button_id:Int):Option[(Boolean, Long)] = {
    window_buttons_info.get(window_button_id) match {
      case Some(MouseButtonPress(_, was_pressed, last_pressed_time)) => Some((was_pressed, last_pressed_time))
      case None => None
    }
  }
  def windowLeftMouse(area:List[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    windowButton(area, 0, repeat_time, mouse_coord => if(!onPause) onBtnDown(mouse_coord), mouse_coord => if(!onPause) onBtnUp(mouse_coord))
  }
  def windowLeftMouseIgnorePause(area:List[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    windowButton(area, 0, repeat_time, mouse_coord => onBtnDown(mouse_coord), mouse_coord => onBtnUp(mouse_coord))
  }
  def windowLeftMouseOnPause(area:List[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    windowButton(area, 0, repeat_time, mouse_coord => if(onPause) onBtnDown(mouse_coord), mouse_coord => if(onPause) onBtnUp(mouse_coord))
  }

  def windowRightMouse(area:List[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    windowButton(area, 1, repeat_time, mouse_coord => if(!onPause) onBtnDown(mouse_coord), mouse_coord => if(!onPause) onBtnUp(mouse_coord))
  }
  def windowRightMouseIgnorePause(area:List[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    windowButton(area, 1, repeat_time, mouse_coord => onBtnDown(mouse_coord), mouse_coord => onBtnUp(mouse_coord))
  }
  def windowRightMouseOnPause(area:List[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    windowButton(area, 1, repeat_time, mouse_coord => if(onPause) onBtnDown(mouse_coord), mouse_coord => if(onPause) onBtnUp(mouse_coord))
  }
  
  def mouseMotion(onMotion: Vec => Any) = {
    on_mouse_motion = mouse_coord => if(!onPause) onMotion(mouse_coord)
    deletion_operations.addOp(() => on_mouse_motion = v => {})
  }
  def mouseMotionIgnorePause(onMotion: Vec => Any) = {
    on_mouse_motion = onMotion
    deletion_operations.addOp(() => on_mouse_motion = v => {})
  }
  def mouseMotionOnPause(onMotion: Vec => Any) = {
    on_mouse_motion = mouse_coord => if(onPause) onMotion(mouse_coord)
    deletion_operations.addOp(() => on_mouse_motion = v => {})
  }

  private def mouseDrag(button_code:Int, onDrag: Vec => Any) = {
    on_mouse_drag_motion(button_code) = onDrag
    deletion_operations.addOp(() => on_mouse_drag_motion -= button_code)
  }

  def leftMouseDrag(onDrag: Vec => Any) = {
    mouseDrag(0, mouse_coord => if(!onPause) onDrag(mouse_coord))
  }
  def leftMouseDragIgnorePause(onDrag: Vec => Any) = {
    mouseDrag(0, onDrag)
  }
  def leftMouseDragOnPause(onDrag: Vec => Any) = {
    mouseDrag(0, mouse_coord => if(onPause) onDrag(mouse_coord))
  }

  def rightMouseDrag(onDrag: Vec => Any) = {
    mouseDrag(1, mouse_coord => if(!onPause) onDrag(mouse_coord))
  }
  def rightMouseDragIgnorePause(onDrag: Vec => Any) = {
    mouseDrag(1, onDrag)
  }
  def rightMouseDragOnPause(onDrag: Vec => Any) = {
    mouseDrag(1, mouse_coord => if(onPause) onDrag(mouse_coord))
  }

  def mouseWheelUp(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = mouse_coord => if(!onPause) onWheelUp(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_up = v => {})
  }
  def mouseWheelUpIgnorePause(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = onWheelUp
    deletion_operations.addOp(() => on_mouse_wheel_up = v => {})
  }
  def mouseWheelUpOnPause(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = mouse_coord => if(onPause) onWheelUp(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_up = v => {})
  }

  def mouseWheelDown(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = mouse_coord => if(!onPause) onWheelDown(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_down = v => {})
  }
  def mouseWheelDownIgnorePause(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = onWheelDown
    deletion_operations.addOp(() => on_mouse_wheel_down = v => {})
  }
  def mouseWheelDownOnPause(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = mouse_coord => if(onPause) onWheelDown(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_down = v => {})
  }

  def checkControls() {
    for {
      (key, key_data) <- keyboard_keys
      SingleKeyEvent(_, repeat_time_func, onKeyDown, onKeyUp) = key_data
      key_press @ KeyPress(_, was_pressed, last_pressed_time) = keyPress(key)
      repeat_time = repeat_time_func()
      is_repeatable = repeat_time > 0
    } {
      if(Keyboard.isKeyDown(key)) {
        if(!was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)) {
          key_press.was_pressed = true
          key_press.last_pressed_time = System.currentTimeMillis()
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
      (button, button_data) <- mouse_buttons
      SingleMouseButtonEvent(_, repeat_time_func, onButtonDown, onButtonUp) = button_data
      mouse_button_press @ MouseButtonPress(_, was_pressed, last_pressed_time) = mouseButtonPress(button)
      repeat_time = repeat_time_func()
      is_repeatable = repeat_time > 0
    } {
      if(Mouse.isButtonDown(button)) {
        if(!was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)) {
          mouse_button_press.was_pressed = true
          mouse_button_press.last_pressed_time = System.currentTimeMillis()
          onButtonDown(mouse_coord)
        }
      } else if(was_pressed) {
        mouse_button_press.was_pressed = false
        onButtonUp(mouse_coord)
      }
    }

    for {
      (window_button_data, mouse_button_press) <- window_buttons
      WindowButtonEvent(area, button, repeat_time_func, onButtonDown, onButtonUp) = window_button_data
      MouseButtonPress(_, was_pressed, last_pressed_time) = mouse_button_press
      repeat_time = repeat_time_func()
      is_repeatable = repeat_time > 0
    } {
      if(Mouse.isButtonDown(button) && mouseOnArea(mouse_coord, area)) {
        if(!was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)) {
          mouse_button_press.was_pressed = true
          mouse_button_press.last_pressed_time = System.currentTimeMillis()
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
      case x if(x > 0) => on_mouse_wheel_up(mouse_coord)
      case x if(x < 0) => on_mouse_wheel_down(mouse_coord)
      case _ =>
    }
  }
}