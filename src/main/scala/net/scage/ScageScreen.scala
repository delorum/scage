package net.scage

import handlers.controller2.{ScageController, SingleController}
import com.weiglewilczek.slf4s.Logger
import handlers.Renderer
import support.ScageProperties._
import handlers.RendererLib._

import java.awt.{BorderLayout, Canvas}
import org.lwjgl.opengl.{GL11, Display}
import java.applet.Applet


// abstract classes instead of traits to make it easy to use with MultiController
abstract class Screen(val unit_name:String = "Scage Screen") extends Scage with Renderer with ScageController {
  private val log = Logger(this.getClass.getName)

  override def run() {
    log.info("starting screen "+unit_name+"...")
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    log.info(unit_name+": run")
    while(is_running && Scage.isAppRunning) {
      checkControls()
      executeActions()
      performRendering()
    }
    executeClears()
    executeDisposes()
    scage_log.info(unit_name+" was stopped")
  }
}
abstract class ScreenApp(
  unit_name:String  = property("app.name", "Scage App"),
  window_width:Int  = property("screen.width", 800),
  window_height:Int = property("screen.height", 600)
) extends Screen(unit_name) with App {
  val app_start_moment = System.currentTimeMillis()
  def msecsFromAppStart = System.currentTimeMillis() - app_start_moment

  override def run() {
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    scage_log.info(unit_name+": run")
    while(is_running && Scage.isAppRunning) {
      checkControls()
      executeActions()
      performRendering()
    }
    renderExitMessage()
    executeClears()
    executeDisposes()
  }

  override def main(args:Array[String]) {
    scage_log.info("starting main screen "+unit_name+"...")
    initgl(window_width, window_height, unit_name)
    drawWelcomeMessages()
    super.main(args)
    run()
    destroygl()
    scage_log.info(unit_name+" was stopped")
    System.exit(0)
  }
}

class ScageScreen(unit_name:String = "Scage Screen") extends Screen(unit_name) with SingleController

class ScageScreenApp(unit_name:String = property("app.name", "Scage App"),
                     window_width:Int  = property("screen.width", 800),
                     window_height:Int = property("screen.height", 600)) extends ScreenApp(unit_name, window_width, window_height) with SingleController

abstract class ScageApplet extends Applet {
  def screen:ScageScreenApp

  /** The Canvas where the LWJGL Display is added */
  var display_parent:Canvas = null

  /** Thread which runs the main game loop */
  var gameThread:Thread = null

  /**
	 * Once the Canvas is created its add notify method will call this method to
	 * start the LWJGL Display and game loop in another thread.
	 */
	def startScage() {
    gameThread = new Thread {
      override def run() {
        Display.setParent(display_parent)
        screen.main(Array[String]())
      }
    }
    gameThread.start()
  }

  /**
   * Tell game loop to stop running, after which the LWJGL Display will be destoryed.
   * The main thread will wait for the Display.destroy() to complete
   */
  def stopScage() {
    screen.stop()
    try {
			gameThread.join()
		} catch {
      case e:InterruptedException =>
			  e.printStackTrace()
		}
  }

  /**
   * Applet Destroy method will remove the canvas, before canvas is destroyed it will notify
   * stopLWJGL() to stop main game loop and to destroy the Display
   */
  override def destroy() {
    remove(display_parent)
    super.destroy()
  }

  /**
	 * initialise applet by adding a canvas to it, this canvas will start the LWJGL Display and game loop
	 * in another thread. It will also stop the game loop and destroy the display on canvas removal when
	 * applet is destroyed.
	 */
	override def init() {
    setLayout(new BorderLayout())
    try {
      display_parent = new Canvas() {
        override def addNotify() {
          super.addNotify()
          startScage()
        }
        override def removeNotify() {
          stopScage()
          super.removeNotify()
        }
      }
      display_parent.setSize(getWidth, getHeight)
      add(display_parent)
      display_parent.setFocusable(true)
      display_parent.requestFocus()
      display_parent.setIgnoreRepaint(true)
      setVisible(true)
    } catch {
      case e:Exception =>
        System.err.println(e)
        throw new RuntimeException("Unable to create display")
    }
  }
}
