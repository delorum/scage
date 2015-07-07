package com.github.dunnololda.scage.handlers.controller2

import com.github.dunnololda.scage.support.Vec
import org.lwjgl.input.{Keyboard, Mouse}
import collection.mutable.ArrayBuffer
import com.github.dunnololda.scage.ScageLib.coordOnArea
import collection.mutable

case class MultiKeyEvent(var was_pressed:Boolean, var last_pressed_time:Long, repeat_time: () => Long, onKeyDown: () => Any, onKeyUp: () => Any)
case class MultiMouseButtonEvent(var was_pressed:Boolean, var last_pressed_time:Long, repeat_time: () => Long, onButtonDown: Vec => Any, onButtonUp: Vec => Any)

trait MultiController extends ScageController {
  private val keyboard_key_events = mutable.HashMap[Int, ArrayBuffer[MultiKeyEvent]]()
  private val anykeys = ArrayBuffer[() => Any]()
  private val mouse_button_events = mutable.HashMap[Int, ArrayBuffer[MultiMouseButtonEvent]]()
  private val mouse_motions = ArrayBuffer[Vec => Any]()
  private val mouse_drag_motions = mutable.HashMap[Int, ArrayBuffer[Vec => Any]]()
  private val mouse_wheel_ups = ArrayBuffer[Vec => Any]()
  private val mouse_wheel_downs = ArrayBuffer[Vec => Any]()

  protected def mappedKeyboardKeys:scala.collection.Set[Int] = keyboard_key_events.keySet
  protected def mappedMouseButtons:scala.collection.Set[Int] = mouse_button_events.keySet

  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    val event = MultiKeyEvent(was_pressed = false, 0, () => repeat_time, () => if(!on_pause) onKeyDown, () => if(!on_pause) onKeyUp)
    if(keyboard_key_events.contains(key_code)) keyboard_key_events(key_code) += event
    else keyboard_key_events(key_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => keyboard_key_events(key_code) -= event, 0)
  }
  def keyIgnorePause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    val event = MultiKeyEvent(was_pressed = false, 0, () => repeat_time, () => onKeyDown, () => onKeyUp)
    if(keyboard_key_events.contains(key_code)) keyboard_key_events(key_code) += event
    else keyboard_key_events(key_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => keyboard_key_events(key_code) -= event, 0)
  }
  def keyOnPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    val event = MultiKeyEvent(was_pressed = false, 0, () => repeat_time, () => if(on_pause) onKeyDown, () => if(on_pause) onKeyUp)
    if(keyboard_key_events.contains(key_code)) keyboard_key_events(key_code) += event
    else keyboard_key_events(key_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => keyboard_key_events(key_code) -= event, 0)
  }

  def anykey(onKeyDown: => Any) = {
    val event = () => if(!on_pause) onKeyDown
    anykeys += event
    deletion_operations.addOp(() => anykeys -= event, 0)
  }
  def anykeyIgnorePause(onKeyDown: => Any) = {
    val event = () => onKeyDown
    anykeys += event
    deletion_operations.addOp(() => anykeys -= event, 0)
  }
  def anykeyOnPause(onKeyDown: => Any) = {
    val event = () => if(on_pause) onKeyDown
    anykeys += event
    deletion_operations.addOp(() => anykeys -= event, 0)
  }

  def mouseCoord = Vec(Mouse.getX, Mouse.getY)
  def isMouseMoved = Mouse.getDX != 0 || Mouse.getDY != 0
  private def mouseButton(button_code:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) = {
    val event = MultiMouseButtonEvent(was_pressed = false, 0, () => repeat_time, onButtonDown, onButtonUp)
    if(mouse_button_events.contains(button_code)) mouse_button_events(button_code) += event
    else mouse_button_events(button_code) = ArrayBuffer(event)
    deletion_operations.addOp(() => mouse_button_events(button_code) -= event, 0)
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

  def leftMouseOnArea(area: => Seq[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(!on_pause && coordOnArea(mouse_coord, area)) onBtnDown(mouse_coord),
                                mouse_coord => if(!on_pause && coordOnArea(mouse_coord, area)) onBtnUp(mouse_coord))
  }
  def leftMouseOnAreaIgnorePause(area: => Seq[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(coordOnArea(mouse_coord, area)) onBtnDown(mouse_coord),
                                mouse_coord => if(coordOnArea(mouse_coord, area)) onBtnUp(mouse_coord))
  }
  def leftMouseOnAreaOnPause(area: => Seq[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(0, repeat_time, mouse_coord => if(on_pause && coordOnArea(mouse_coord, area)) onBtnDown(mouse_coord),
                                mouse_coord => if(on_pause && coordOnArea(mouse_coord, area)) onBtnUp(mouse_coord))
  }

  def leftMouseOnRect(leftup:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(leftup, leftup + Vec(width, 0), leftup + Vec(width, -height), leftup + Vec(0, -height))
    leftMouseOnArea(area, repeat_time, onBtnDown, onBtnUp)
  }
  def leftMouseOnRectIgnorePause(leftup:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(leftup, leftup + Vec(width, 0), leftup + Vec(width, -height), leftup + Vec(0, -height))
    leftMouseOnAreaIgnorePause(area, repeat_time, onBtnDown, onBtnUp)
  }
  def leftMouseOnRectOnPause(leftup:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(leftup, leftup + Vec(width, 0), leftup + Vec(width, -height), leftup + Vec(0, -height))
    leftMouseOnAreaOnPause(area, repeat_time, onBtnDown, onBtnUp)
  }

  def leftMouseOnRectCentered(coord:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(coord + Vec(-width/2,  height/2), coord + Vec( width/2,  height/2), coord + Vec( width/2, -height/2), coord + Vec(-width/2, -height/2))
    leftMouseOnArea(area, repeat_time, onBtnDown, onBtnUp)
  }
  def leftMouseOnRectCenteredIgnorePause(coord:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(coord + Vec(-width/2,  height/2), coord + Vec( width/2,  height/2), coord + Vec( width/2, -height/2), coord + Vec(-width/2, -height/2))
    leftMouseOnAreaIgnorePause(area, repeat_time, onBtnDown, onBtnUp)
  }
  def leftMouseOnRectCenteredOnPause(coord:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(coord + Vec(-width/2,  height/2), coord + Vec( width/2,  height/2), coord + Vec( width/2, -height/2), coord + Vec(-width/2, -height/2))
    leftMouseOnAreaOnPause(area, repeat_time, onBtnDown, onBtnUp)
  }

  def rightMouseOnArea(area: => Seq[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(!on_pause && coordOnArea(mouse_coord, area)) onBtnDown(mouse_coord),
                                mouse_coord => if(!on_pause && coordOnArea(mouse_coord, area)) onBtnUp(mouse_coord))
  }
  def rightMouseOnAreaIgnorePause(area: => Seq[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(coordOnArea(mouse_coord, area)) onBtnDown(mouse_coord),
                                mouse_coord => if(coordOnArea(mouse_coord, area)) onBtnUp(mouse_coord))
  }
  def rightMouseOnAreaOnPause(area: => Seq[Vec], repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    mouseButton(1, repeat_time, mouse_coord => if(on_pause && coordOnArea(mouse_coord, area)) onBtnDown(mouse_coord),
                                mouse_coord => if(on_pause && coordOnArea(mouse_coord, area)) onBtnUp(mouse_coord))
  }

  def rightMouseOnRect(leftup:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(leftup, leftup + Vec(width, 0), leftup + Vec(width, -height), leftup + Vec(0, -height))
    rightMouseOnArea(area, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouseOnRectIgnorePause(leftup:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(leftup, leftup + Vec(width, 0), leftup + Vec(width, -height), leftup + Vec(0, -height))
    rightMouseOnAreaIgnorePause(area, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouseOnRectOnPause(leftup:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(leftup, leftup + Vec(width, 0), leftup + Vec(width, -height), leftup + Vec(0, -height))
    rightMouseOnAreaOnPause(area, repeat_time, onBtnDown, onBtnUp)
  }

  def rightMouseOnRectCentered(coord:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(coord + Vec(-width/2,  height/2), coord + Vec( width/2,  height/2), coord + Vec( width/2, -height/2), coord + Vec(-width/2, -height/2))
    rightMouseOnArea(area, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouseOnRectCenteredIgnorePause(coord:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(coord + Vec(-width/2,  height/2), coord + Vec( width/2,  height/2), coord + Vec( width/2, -height/2), coord + Vec(-width/2, -height/2))
    rightMouseOnAreaIgnorePause(area, repeat_time, onBtnDown, onBtnUp)
  }
  def rightMouseOnRectCenteredOnPause(coord:Vec, width:Float, height:Float, repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}) = {
    val area = List(coord + Vec(-width/2,  height/2), coord + Vec( width/2,  height/2), coord + Vec( width/2, -height/2), coord + Vec(-width/2, -height/2))
    rightMouseOnAreaOnPause(area, repeat_time, onBtnDown, onBtnUp)
  }

  def mouseMotion(onMotion: Vec => Any) = {
    val event = {mouse_coord:Vec => if(!on_pause) onMotion(mouse_coord)}
    mouse_motions += event
    deletion_operations.addOp(() => mouse_motions -= event, 0)
  }
  def mouseMotionIgnorePause(onMotion: Vec => Any) = {
    mouse_motions += onMotion
    deletion_operations.addOp(() => mouse_motions -= onMotion, 0)
  }
  def mouseMotionOnPause(onMotion: Vec => Any) = {
    val event = {mouse_coord:Vec => if(on_pause) onMotion(mouse_coord)}
    mouse_motions += event
    deletion_operations.addOp(() => mouse_motions -= event, 0)
  }

  private def mouseDrag(button_code:Int, onDrag: Vec => Any) = {
    if(mouse_drag_motions.contains(button_code)) mouse_drag_motions(button_code) += onDrag
    else mouse_drag_motions(button_code) = ArrayBuffer(onDrag)
    deletion_operations.addOp(() => mouse_drag_motions(button_code) -= onDrag, 0)
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
    val event = {mouse_coord:Vec => if(!on_pause) onWheelUp(mouse_coord)}
    mouse_wheel_ups += event
    deletion_operations.addOp(() => mouse_wheel_ups -= event, 0)
  }
  def mouseWheelUpIgnorePause(onWheelUp: Vec => Any) = {
    mouse_wheel_ups += onWheelUp
    deletion_operations.addOp(() => mouse_wheel_ups -= onWheelUp, 0)
  }
  def mouseWheelUpOnPause(onWheelUp: Vec => Any) = {
    val event = {mouse_coord:Vec => if(on_pause) onWheelUp(mouse_coord)}
    mouse_wheel_ups += event
    deletion_operations.addOp(() => mouse_wheel_ups -= event, 0)
  }

  def mouseWheelDown(onWheelDown: Vec => Any) = {
    val event = {mouse_coord:Vec => if(!on_pause) onWheelDown(mouse_coord)}
    mouse_wheel_downs += event
    deletion_operations.addOp(() => mouse_wheel_downs -= event, 0)
  }
  def mouseWheelDownIgnorePause(onWheelDown: Vec => Any) = {
    mouse_wheel_downs += onWheelDown
    deletion_operations.addOp(() => mouse_wheel_downs -= onWheelDown, 0)
  }
  def mouseWheelDownOnPause(onWheelDown: Vec => Any) = {
    val event = {mouse_coord:Vec => if(on_pause) onWheelDown(mouse_coord)}
    mouse_wheel_downs += event
    deletion_operations.addOp(() => mouse_wheel_downs -= event, 0)
  }

  def checkControls() {
    for {
      (key, events_for_key) <- keyboard_key_events
      key_press @ KeyPress(_, was_pressed, _, _) <- innerKeyPress(key)
    } {
      if(Keyboard.isKeyDown(key)) {
        if(!was_pressed) {
          key_press.was_pressed = true
          key_press.pressed_start_time = System.currentTimeMillis()
        }
        for {
          key_data <- events_for_key
          MultiKeyEvent(_, last_pressed_time, repeat_time_func, onKeyDown, onKeyUp) = key_data
          repeat_time = repeat_time_func()
          is_repeatable = repeat_time > 0
          if !was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)
        } {
          key_data.was_pressed = true
          key_data.last_pressed_time = System.currentTimeMillis()
          onKeyDown()
        }
      } else if(was_pressed) {
        key_press.was_pressed = false
        for {
          key_data <- events_for_key
          MultiKeyEvent(_, _, _, _, onKeyUp) = key_data
        } {
          key_data.was_pressed = false
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
      (button, events_for_button) <- mouse_button_events
      mouse_button_press @ MouseButtonPress(_, was_pressed, _, _) <- innerMouseButtonPress(button)
    } {
      if(Mouse.isButtonDown(button)) {
        if(!was_pressed) {
          mouse_button_press.was_pressed = true
          mouse_button_press.pressed_start_time = System.currentTimeMillis()
        }
        for {
          button_data <- events_for_button
          MultiMouseButtonEvent(_, last_pressed_time, repeat_time_func, onButtonDown, _) = button_data
          repeat_time = repeat_time_func()
          is_repeatable = repeat_time > 0
          if !was_pressed || (is_repeatable && System.currentTimeMillis() - last_pressed_time > repeat_time)
        } {
          button_data.was_pressed = true
          button_data.last_pressed_time = System.currentTimeMillis()
          onButtonDown(mouse_coord)
        }
      } else if(was_pressed) {
        mouse_button_press.was_pressed = false
        for {
          button_data <- events_for_button
          MultiMouseButtonEvent(_, _, _, _, onButtonUp) = button_data
        } {
          button_data.was_pressed = false
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
      case x if x > 0 => mouse_wheel_ups.foreach(onWheelUp => onWheelUp(mouse_coord))
      case x if x < 0 => mouse_wheel_downs.foreach(onWheelDown => onWheelDown(mouse_coord))
      case _ =>
    }
  }
}