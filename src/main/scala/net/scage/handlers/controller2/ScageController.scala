package net.scage.handlers.controller2

import net.scage.support.Vec
import collection.mutable.{ArrayBuffer, HashMap}
import com.weiglewilczek.slf4s.Logger
import net.scage.{ScageOperation, Scage}
import collection.mutable

case class KeyPress(key_code:Int, var was_pressed:Boolean, var last_pressed_time:Long)
case class MouseButtonPress(button_code:Int, var was_pressed:Boolean, var last_pressed_time:Long)

object ScageController {
  private val key_presses = HashMap[Int, KeyPress]()
  private val mouse_button_presses = HashMap[Int, MouseButtonPress]()
}

trait ScageController extends Scage {
  private val log = Logger(this.getClass.getName)

  protected def keyPress(key_code:Int):KeyPress = {
    ScageController.key_presses.get(key_code) match {
      case Some(kp:KeyPress) => kp
      case None =>
        val kp = KeyPress(key_code, was_pressed = false, 0L)
        ScageController.key_presses += (key_code -> kp)
        kp
    }
  }

  protected def mouseButtonPress(mouse_button:Int):MouseButtonPress = {
    ScageController.mouse_button_presses.get(mouse_button) match {
      case Some(mbp:MouseButtonPress) => mbp
      case None =>
        val mbp = MouseButtonPress(mouse_button, was_pressed = false, 0L)
        ScageController.mouse_button_presses += (mouse_button -> mbp)
        mbp
    }
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
        case some_operation @ Some(ScageOperation(_, op)) =>
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

  def delControl(control_id:Int) = {deletion_operations.delOperation(control_id)}
  def delControls(control_ids:Int*) {deletion_operations.delOperations(control_ids:_*)}
  def delAllControls() {deletion_operations.delAllOperations()}
  def delAllControlsExcept(except_control_ids:Int*) {deletion_operations.delAllOperationsExcept(except_control_ids:_*)}
}

trait SynchronizedScageController extends ScageController {
  class SynchronizedControlDeletionsContainer extends ControlDeletionsContainer {
    override protected val _operations = new ArrayBuffer[ScageOperation] with mutable.SynchronizedBuffer[ScageOperation]
  }
  override def controlDeletersContainer:ControlDeletionsContainer = new SynchronizedControlDeletionsContainer
}

