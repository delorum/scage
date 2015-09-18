package com.github.dunnololda.scage.handlers.controller2

import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.scage.{ScageOperation, Scage}
import org.lwjgl.input.{Mouse, Keyboard}
import com.github.dunnololda.scage.ScageLib.coordOnArea
import collection.mutable
import com.github.dunnololda.mysimplelogger.MySimpleLogger

case class KeyPress(key_code:Int, var was_pressed:Boolean, var pressed_start_time:Long, var last_pressed_time:Long) {
  def immutable:ImmutableKeyPress = ImmutableKeyPress(key_code, was_pressed, pressed_start_time, last_pressed_time)
}
case class ImmutableKeyPress(key_code:Int, was_pressed:Boolean, pressed_start_time:Long, last_pressed_time:Long)
case class MouseButtonPress(button_code:Int, var was_pressed:Boolean, var pressed_start_time:Long, var last_pressed_time:Long) {
  def immutable:ImmutableMouseButtonPress = ImmutableMouseButtonPress(button_code, was_pressed, pressed_start_time, last_pressed_time)
}
case class ImmutableMouseButtonPress(button_code:Int, was_pressed:Boolean, pressed_start_time:Long, last_pressed_time:Long)

object ScageController {
  private val key_presses = mutable.HashMap[Int, KeyPress]()
  private val mouse_button_presses = mutable.HashMap[Int, MouseButtonPress]()
}

trait ScageController extends Scage {
  private val log = MySimpleLogger(this.getClass.getName)

  protected def mappedKeyboardKeys:scala.collection.Set[Int]
  protected def mappedMouseButtons:scala.collection.Set[Int]

  protected def innerKeyPress(key_code:Int):Option[KeyPress] = {
    ScageController.key_presses.get(key_code) match {
      case skp @ Some(kp:KeyPress) => skp
      case None if mappedKeyboardKeys.contains(key_code)=>
        val kp = KeyPress(key_code, was_pressed = false, 0L, 0L)
        ScageController.key_presses += (key_code -> kp)
        Some(kp)
      case _ => None
    }
  }

  def keyPress(key_code:Int):Option[ImmutableKeyPress] = innerKeyPress(key_code).map(_.immutable)

  protected def innerMouseButtonPress(mouse_button:Int):Option[MouseButtonPress] = {
    ScageController.mouse_button_presses.get(mouse_button) match {
      case smbp @ Some(mbp:MouseButtonPress) => smbp
      case None if mappedMouseButtons.contains(mouse_button) =>
        val mbp = MouseButtonPress(mouse_button, was_pressed = false, 0L, 0L)
        ScageController.mouse_button_presses += (mouse_button -> mbp)
        Some(mbp)
      case _ => None
    }
  }

  def mouseButtonPress(key_code:Int):Option[ImmutableMouseButtonPress] = innerMouseButtonPress(key_code).map(_.immutable)

  def keyPressed(key_code:Int):Boolean = {
    /*val KeyPress(_, was_pressed, _) = keyPress(key_code)
    was_pressed*/
    Keyboard.isKeyDown(key_code)
  }

  def leftMousePressed:Boolean = {
    /*val MouseButtonPress(_, was_pressed, _) = mouseButtonPress(0)
    was_pressed*/
    Mouse.isButtonDown(0)
  }

  def rightMousePressed:Boolean = {
    /*val MouseButtonPress(_, was_pressed, _) = mouseButtonPress(1)
    was_pressed*/
    Mouse.isButtonDown(1)
  }

  def mouseOnArea(area:List[Vec]):Boolean = {
    coordOnArea(mouseCoord, area)
  }
  
  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int
  def keyIgnorePause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int
  def keyOnPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int

  def anykey(onKeyDown: => Any):Int
  def anykeyIgnorePause(onKeyDown: => Any):Int
  def anykeyOnPause(onKeyDown: => Any):Int

  def mouseCoord:Vec
  def isMouseMoved:Boolean

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int
  def leftMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int
  def leftMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int
  def rightMouseIgnorePause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int
  def rightMouseOnPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int

  def mouseMotion(onMotion: Vec => Any):Int
  def mouseMotionIgnorePause(onMotion: Vec => Any):Int
  def mouseMotionOnPause(onMotion: Vec => Any):Int

  def leftMouseDrag(onDrag: Vec => Any):Int
  def leftMouseDragIgnorePause(onDrag: Vec => Any):Int
  def leftMouseDragOnPause(onDrag: Vec => Any):Int

  def rightMouseDrag(onDrag: Vec => Any):Int
  def rightMouseDragIgnorePause(onDrag: Vec => Any):Int
  def rightMouseDragOnPause(onDrag: Vec => Any):Int

  def mouseWheelUp(onWheelUp: Vec => Any):Int
  def mouseWheelUpIgnorePause(onWheelUp: Vec => Any):Int
  def mouseWheelUpOnPause(onWheelUp: Vec => Any):Int

  def mouseWheelDown(onWheelDown: Vec => Any):Int
  def mouseWheelDownIgnorePause(onWheelDown: Vec => Any):Int
  def mouseWheelDownOnPause(onWheelDown: Vec => Any):Int

  private[scage] def checkControls()

  class ControlDeletionsContainer extends DefaultOperationContainer("control_deleters") {
    override protected def _delOperation(op_id:Int, show_warnings:Boolean) = {
      removeOperation(op_id) match {
        case some_operation @ Some(ScageOperation(_, op, _)) =>
          log.debug("deleted operation with id "+op_id+" from the container "+name)
          operation_mapping -= op_id
          op()
          some_operation
        case None =>
          if(show_warnings) log.warn("operation with id "+op_id+" not found in the container "+name)
          None
      }
    }
  }
  def controlDeletersContainer:ControlDeletionsContainer = new ControlDeletionsContainer

  private[scage] val deletion_operations = controlDeletersContainer

  def delControl(control_id:Int) = {deletion_operations.delOperationImmediately(control_id)}
  def delControls(control_ids:Int*) {deletion_operations.delOperationsImmediately(control_ids:_*)}
  def delAllControls() {deletion_operations.delAllOperationsImmediately()}
  def delAllControlsExcept(except_control_ids:Int*) {deletion_operations.delAllOperationsExceptImmediately(except_control_ids:_*)}
}

