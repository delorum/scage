package net.scage.handlers.controller2

import net.scage.support.Vec
import collection.mutable.HashMap
import org.lwjgl.input.{Keyboard, Mouse}

case class SingleKeyEvent(key_code:Int, repeat_time: () => Long, onKeyDown: () => Any, onKeyUp: () => Any)
case class SingleMouseButtonEvent(button_code:Int, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)

trait SingleController extends ScageController {
  private var keyboard_keys = HashMap[Int, SingleKeyEvent]()  // was_pressed, last_pressed_time, repeat_time, onKeyDown, onKeyUp
  private var anykey: () => Any = () => {}
  private var mouse_buttons = HashMap[Int, SingleMouseButtonEvent]()
  private var on_mouse_motion: Vec => Any = v => {}
  private var on_mouse_drag_motion = HashMap[Int, Vec => Any]()
  private var on_mouse_wheel_up: Vec => Any = v => {}
  private var on_mouse_wheel_down: Vec => Any = v => {}

  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) {
    keyboard_keys(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => onKeyDown, () => onKeyUp)
  }
  def anykey(onKeyDown: => Any) {
    anykey = () => onKeyDown
  }

  def mouseCoord = Vec(Mouse.getX, Mouse.getY)
  def isMouseMoved = Mouse.getDX != 0 || Mouse.getDY != 0
  private def mouseButton(button_code:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) {
    mouse_buttons(button_code) = SingleMouseButtonEvent(button_code, () => repeat_time, onButtonDown, onButtonUp)
  }
  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) {
    mouseButton(0, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) {
    mouseButton(1, repeat_time, onBtnDown, onBtnUp)
  }
  def mouseMotion(onMotion: Vec => Any) {
    on_mouse_motion = onMotion
  }
  private def mouseDrag(button_code:Int, onDrag: Vec => Any) {
    on_mouse_drag_motion(button_code) = onDrag
  }
  def leftMouseDrag(onDrag: Vec => Any) {
    mouseDrag(0, onDrag)
  }
  def rightMouseDrag(onDrag: Vec => Any) {
    mouseDrag(1, onDrag)
  }
  def mouseWheelUp(onWheelUp: Vec => Any) {
    on_mouse_wheel_up = onWheelUp
  }
  def mouseWheelDown(onWheelDown: Vec => Any) {
    on_mouse_wheel_down = onWheelDown
  }

  def delKeys(key_codes_to_delete: Int*) {
    keyboard_keys --= key_codes_to_delete
  }
  def delAnyKey() {
    anykey = () => {}
  }
  def delAllKeys() {
    keyboard_keys.clear()
  }

  def delMouseButtons(mouse_buttons_to_delete:Int*) {
    mouse_buttons --= mouse_buttons_to_delete
  }
  def delAllMouseButtons() {
    mouse_buttons.clear()
  }
  def delMouseMotion() {
    on_mouse_motion = v => {}
  }
  def delMouseDrags(mouse_buttons_to_delete:Int*) {
    on_mouse_drag_motion --= mouse_buttons_to_delete
  }
  def delAllMouseDrags() {
    on_mouse_drag_motion.clear()
  }
  def delMouseWheelUp() {
    on_mouse_wheel_up = v => {}
  }
  def delMouseWheelDown() {
    on_mouse_wheel_down = v => {}
  }
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