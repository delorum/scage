package net.scage

import handlers.controller2.{ScageController, SingleController}
import com.weiglewilczek.slf4s.Logger
import handlers.Renderer
import support.ScageProperties._
import handlers.RendererLib._

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

/*class MultiControlledScreen(unit_name:String = "Scage App", is_main_unit:Boolean = false, properties:String = "")
extends Screen(unit_name, is_main_unit, properties) with MultiController*/
