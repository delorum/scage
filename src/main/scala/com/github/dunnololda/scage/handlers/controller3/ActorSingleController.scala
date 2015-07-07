package com.github.dunnololda.scage.handlers.controller3

import akka.actor.{Props, Actor, ActorSystem}
import akka.util.Timeout
import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.handlers.controller2._
import com.github.dunnololda.scage.support.Vec
import org.lwjgl.input.{Mouse, Keyboard}
import org.lwjgl.opengl.Display
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.collection.{mutable, Set}
import concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
object ControllerActorSystemHolder {
  lazy val controller_system = ActorSystem("controller")
}

case object CheckControls
case class AddKey(key_code:Int)
case class RemoveKey(key_code:Int)
case object GetAllKeysHistoryAndReset
case class GetKeyHistoryAndReset(key_code:Int)

class ControllerActor extends Actor {
  private val log = MySimpleLogger(this.getClass.getName)
  private val key_presses_history = mutable.HashMap[Int, ArrayBuffer[Boolean]]()


  override def preStart(): Unit = {
    log.info(s"starting actor ${self.path}")
    context.system.scheduler.schedule(0.seconds, 10.milliseconds) {
      self ! CheckControls
    }
  }

  override def postStop(): Unit = {
    log.info(s"actor ${self.path} died!")
  }

  private var start = System.currentTimeMillis()

  def receive = {
    case AddKey(key_code) =>
      //log.info(s"actor ${self.path} adding key $key_code")
      key_presses_history += (key_code -> ArrayBuffer[Boolean]())
    case RemoveKey(key_code) =>
      //log.info(s"actor ${self.path} removing key $key_code")
      key_presses_history -= key_code
    case CheckControls =>
      log.info(s"[${System.currentTimeMillis() - start}] check controls")
      start = System.currentTimeMillis()
      Display.processMessages()
      //log.info(s"actor ${self.path} check controls")
      key_presses_history.foreach {
        case (key_code, history) =>
          if(Keyboard.isKeyDown(key_code)) {
            history += true
            if(key_code == 2) {
              log.info("pressed")
            }
          } else {
            history += false
          }
      }
    case GetAllKeysHistoryAndReset =>
      sender ! key_presses_history.map(kv => {
        val kv_2_list = kv._2.toList
        kv._2.clear()
        (kv._1, kv_2_list)
      }).toMap
    case GetKeyHistoryAndReset(key_code) =>
      val history = key_presses_history.get(key_code) match {
        case Some(h) =>
          val l = h.toList
          h.clear()
          l
        case None => Nil
      }
      /*if(key_code == 2) {
        log.info(s"actor ${self.path} get history for key $key_code: $history")
      }*/
      sender ! history
  }
}

trait ActorSingleController extends ScageController {
  private val controller_actor = ControllerActorSystemHolder.controller_system.actorOf(Props(new ControllerActor))

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
    controller_actor ! AddKey(key_code)
    deletion_operations.addOp(() => {
      keyboard_key_events -= key_code
      controller_actor ! RemoveKey(key_code)
    }, 0)
  }
  def keyIgnorePause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}) = {
    keyboard_key_events(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => onKeyDown, () => onKeyUp)
    controller_actor ! AddKey(key_code)
    deletion_operations.addOp(() => {
      keyboard_key_events -= key_code
      controller_actor ! RemoveKey(key_code)
    }, 0)
  }
  def keyOnPause(key_code:Int, repeat_time: => Long = 0, onKeyDown: => Any, onKeyUp: => Any = {}):Int = {
    keyboard_key_events(key_code) = SingleKeyEvent(key_code, () => repeat_time, () => if(on_pause) onKeyDown, () => if(on_pause) onKeyUp)
    controller_actor ! AddKey(key_code)
    deletion_operations.addOp(() => {
      keyboard_key_events -= key_code
      controller_actor ! RemoveKey(key_code)
    }, 0)
  }

  def anykey(onKeyDown: => Any) = {
    anykey = () => if(!on_pause) onKeyDown
    deletion_operations.addOp(() => anykey = () => {}, 0)
  }
  def anykeyIgnorePause(onKeyDown: => Any) = {
    anykey = () => onKeyDown
    deletion_operations.addOp(() => anykey = () => {}, 0)
  }
  def anykeyOnPause(onKeyDown: => Any) = {
    anykey = () => if(on_pause) onKeyDown
    deletion_operations.addOp(() => anykey = () => {}, 0)
  }

  def mouseCoord = Vec(Mouse.getX, Mouse.getY)
  def isMouseMoved = Mouse.getDX != 0 || Mouse.getDY != 0
  private def mouseButton(button_code:Int, repeat_time: => Long = 0, onButtonDown: Vec => Any, onButtonUp: Vec => Any = Vec => {}) = {
    mouse_button_events(button_code) = SingleMouseButtonEvent(button_code, () => repeat_time, onButtonDown, onButtonUp)
    deletion_operations.addOp(() => mouse_button_events -= button_code, 0)
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
    deletion_operations.addOp(() => on_mouse_motion = v => {}, 0)
  }
  def mouseMotionIgnorePause(onMotion: Vec => Any) = {
    on_mouse_motion = onMotion
    deletion_operations.addOp(() => on_mouse_motion = v => {}, 0)
  }
  def mouseMotionOnPause(onMotion: Vec => Any) = {
    on_mouse_motion = mouse_coord => if(on_pause) onMotion(mouse_coord)
    deletion_operations.addOp(() => on_mouse_motion = v => {}, 0)
  }

  private def mouseDrag(button_code:Int, onDrag: Vec => Any) = {
    on_mouse_drag_motion(button_code) = onDrag
    deletion_operations.addOp(() => on_mouse_drag_motion -= button_code, 0)
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
    deletion_operations.addOp(() => on_mouse_wheel_up = v => {}, 0)
  }
  def mouseWheelUpIgnorePause(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = onWheelUp
    deletion_operations.addOp(() => on_mouse_wheel_up = v => {}, 0)
  }
  def mouseWheelUpOnPause(onWheelUp: Vec => Any) = {
    on_mouse_wheel_up = mouse_coord => if(on_pause) onWheelUp(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_up = v => {}, 0)
  }

  def mouseWheelDown(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = mouse_coord => if(!on_pause) onWheelDown(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_down = v => {}, 0)
  }
  def mouseWheelDownIgnorePause(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = onWheelDown
    deletion_operations.addOp(() => on_mouse_wheel_down = v => {}, 0)
  }
  def mouseWheelDownOnPause(onWheelDown: Vec => Any) = {
    on_mouse_wheel_down = mouse_coord => if(on_pause) onWheelDown(mouse_coord)
    deletion_operations.addOp(() => on_mouse_wheel_down = v => {}, 0)
  }

  def checkControls() {
    for {
      (key, key_data) <- keyboard_key_events
      SingleKeyEvent(_, repeat_time_func, onKeyDown, onKeyUp) = key_data
      key_press <- innerKeyPress(key)
      key_history <- Await.result(controller_actor.?(GetKeyHistoryAndReset(key))(Timeout(100.millis)).mapTo[List[Boolean]], 100.millis)
    } {
      if(key_history) {
        val repeat_time = repeat_time_func()
        val is_repeatable = repeat_time > 0
        if(!key_press.was_pressed || (is_repeatable && System.currentTimeMillis() - key_press.last_pressed_time > repeat_time)) {
          if(!key_press.was_pressed) key_press.pressed_start_time = System.currentTimeMillis()
          key_press.was_pressed = true
          key_press.last_pressed_time = System.currentTimeMillis()
          onKeyDown()
        }
      } else if(key_press.was_pressed) {
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
      case x if x > 0 => on_mouse_wheel_up(mouse_coord)
      case x if x < 0 => on_mouse_wheel_down(mouse_coord)
      case _ =>
    }
  }
}
