package net.scage.handlers.controller2

import net.scage.support.Vec
import net.scage.Scage
import collection.mutable.HashMap
import com.weiglewilczek.slf4s.Logger

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
  
  def key(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int
  def keyNoPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int

  def anykey(onKeyDown: => Any):Int
  def anykeyNoPause(onKeyDown: => Any):Int

  def mouseCoord:Vec
  def isMouseMoved:Boolean

  def leftMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int
  def leftMouseNoPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int

  def rightMouse(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int
  def rightMouseNoPause(repeat_time: => Long = 0, onBtnDown: Vec => Any, onBtnUp: Vec => Any = Vec => {}):Int

  def mouseMotion(onMotion: Vec => Any):Int
  def mouseMotionNoPause(onMotion: Vec => Any):Int

  def leftMouseDrag(onDrag: Vec => Any):Int
  def leftMouseDragNoPause(onDrag: Vec => Any):Int

  def rightMouseDrag(onDrag: Vec => Any):Int
  def rightMouseDragNoPause(onDrag: Vec => Any):Int

  def mouseWheelUp(onWheelUp: Vec => Any):Int
  def mouseWheelUpNoPause(onWheelUp: Vec => Any):Int

  def mouseWheelDown(onWheelDown: Vec => Any):Int
  def mouseWheelDownNoPause(onWheelDown: Vec => Any):Int

  def checkControls()
  
  private[controller2] val deletion_operations = HashMap[Int, () => Any]()

  def delControls(control_ids:Int*):Boolean = {
    val result = control_ids.foldLeft(true)((overall_result, control_id) => {
      val deletion_result = deletion_operations.get(control_id) match {
        case Some(op) =>
          op()
          log.debug("deleted control with id "+control_id)
          true
        case None =>
          log.warn("control with id "+control_id+" not found so wasn't deleted")
          false
      }
      overall_result && deletion_result
    })
    deletion_operations --= control_ids
    result
  }
  def delAllControls() {
    for(op <- deletion_operations.values) op()
    deletion_operations.clear()
    log.info("deleted all control operations")
  }
  def delAllControlsExcept(control_ids:Int*) {
    delControls(deletion_operations.keys.filter(!control_ids.contains(_)).toSeq:_*)
  }

  object ControlOperations extends Enumeration {
    val Control = Value
  }

  override def delOperation(operation_id:Int) = {
    operations_mapping.get(operation_id) match {
      case Some(operation_type) => {
        operation_type match {
          case ControlOperations.Control => delControls(operation_id)
          case _ => super.delOperation(operation_id)
        }
      }
      case None =>  super.delOperation(operation_id)
    }
  }
  
  override def delAllOperations() {
    delAllControls()
    super.delAllOperations()
  }
  override def delAllOperationsExcept(operation_ids:Int*) {
    delAllControlsExcept(operation_ids:_*)
    super.delAllOperationsExcept(operation_ids:_*)
  }
}

