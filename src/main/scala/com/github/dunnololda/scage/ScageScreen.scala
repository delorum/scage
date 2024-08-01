package com.github.dunnololda.scage

import java.applet.Applet
import java.awt.{BorderLayout, Canvas}
import com.github.dunnololda.cli.Imports._
import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.handlers._
import com.github.dunnololda.scage.handlers.controller2.{ScageController, SingleController}
import com.github.dunnololda.scage.handlers.controller3.{ActorSingleController, ControllerActorSystem}
import com.github.dunnololda.scage.support.XInitThreadsCaller.xInitThreads
import org.lwjgl.opengl.Display

// abstract classes instead of traits to make it easy to use with MultiController
abstract class Screen(val unit_name: String = "Scage Screen") extends Scage with Renderer with ScageController {
  private val log = MySimpleLogger(this.getClass.getName)

  override def run(): Unit = {
    log.info("starting screen " + unit_name + "...")
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      checkControlsAndExecuteActions()
      performRendering()
    }
    executeClears()
    executeDisposes()
    scage_log.info(unit_name + " was stopped")
  }
}

abstract class ScreenD(val unit_name: String = "Scage Screen") extends Scage with RendererD with ScageController {
  private val log = MySimpleLogger(this.getClass.getName)

  override def run(): Unit = {
    log.info("starting screen " + unit_name + "...")
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      checkControlsAndExecuteActions()
      performRendering()
    }
    executeClears()
    executeDisposes()
    scage_log.info(unit_name + " was stopped")
  }
}

abstract class ScreenApp(
                          title: String = property("app.name", "Scage App"),
                          width: Int = property("screen.width", 800),
                          height: Int = property("screen.height", 600)
                        ) extends Screen(title) with Cli {
  val app_start_moment = System.currentTimeMillis()

  def msecsFromAppStart: Long = System.currentTimeMillis() - app_start_moment

  override def run(): Unit = {
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    scage_log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      checkControlsAndExecuteActions()
      performRendering()
    }
    RendererLib.renderExitMessage()
    executeClears()
    executeDisposes()
  }

  override def main(args: Array[String]) {
    scage_log.info("starting main screen " + title + "...")
    RendererLib.initgl(width, height, title)
    RendererLib.drawWelcomeMessages()
    super.main(args)
    run()
    RendererLib.destroygl()
    scage_log.info(title + " was stopped")
    System.exit(0) // need explicit exit for the app's utilizing NetServer/NetClient as they have actors
  }
}

abstract class ScreenAppD(
                           title: String = property("app.name", "Scage App"),
                           width: Int = property("screen.width", 800),
                           height: Int = property("screen.height", 600)
                         ) extends ScreenD(title) with Cli {
  val app_start_moment = System.currentTimeMillis()

  def msecsFromAppStart: Long = System.currentTimeMillis() - app_start_moment

  override def run(): Unit = {
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    scage_log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      checkControlsAndExecuteActions()
      performRendering()
    }
    RendererLibD.renderExitMessage()
    executeClears()
    executeDisposes()
  }

  override def main(args: Array[String]) {
    scage_log.info("starting main screen " + title + "...")
    RendererLibD.initgl(width, height, title)
    RendererLibD.drawWelcomeMessages()
    super.main(args)
    run()
    RendererLibD.destroygl()
    scage_log.info(title + " was stopped")
    System.exit(0) // need explicit exit for the app's utilizing NetServer/NetClient as they have actors
  }
}

abstract class ScreenAppMT(
                            title: String = property("app.name", "Scage App"),
                            width: Int = property("screen.width", 800),
                            height: Int = property("screen.height", 600)
                          ) extends Screen(title) with Cli {
  val app_start_moment = System.currentTimeMillis()

  def msecsFromAppStart: Long = System.currentTimeMillis() - app_start_moment

  override def run(): Unit = {
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    scage_log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      checkControlsAndExecuteActions()
      performRendering()
    }
    RendererLib.renderExitMessage()
    executeClears()
    executeDisposes()
  }

  override def main(args: Array[String]) {
    scage_log.info("starting main screen " + title + "...")
    xInitThreads(scage_log)
    ControllerActorSystem.createControllerActor()
    ControllerActorSystem.initGLAndReleaseContext(width, height, title)
    Display.makeCurrent()
    RendererLib.drawWelcomeMessages()
    ControllerActorSystem.startCheckControls()
    super.main(args)
    run()
    ControllerActorSystem.shutDownAndAwaitTermination()
    RendererLib.destroygl()
    scage_log.info(title + " was stopped")
    //System.exit(0)  // need explicit exit for the app's utilizing NetServer/NetClient as they have actors
  }
}

abstract class ScreenAppDMT(
                             title: String = property("app.name", "Scage App"),
                             width: Int = property("screen.width", 800),
                             height: Int = property("screen.height", 600)
                           ) extends ScreenD(title) with Cli {
  val app_start_moment = System.currentTimeMillis()

  def msecsFromAppStart: Long = System.currentTimeMillis() - app_start_moment

  override def run(): Unit = {
    executePreinits()
    executeInits()
    is_running = true
    prepareRendering()
    scage_log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      checkControlsAndExecuteActions()
      performRendering()
    }
    RendererLibD.renderExitMessage()
    executeClears()
    executeDisposes()
  }

  override def main(args: Array[String]) {
    scage_log.info("starting main screen " + title + "...")
    //xInitThreads(scage_log)
    ControllerActorSystem.createControllerActor()
    ControllerActorSystem.initGLAndReleaseContext(width, height, title)
    Display.makeCurrent()
    RendererLib.drawWelcomeMessages()
    ControllerActorSystem.startCheckControls()
    super.main(args)
    run()
    ControllerActorSystem.shutDownAndAwaitTermination()
    RendererLibD.destroygl()
    scage_log.info(title + " was stopped")
    System.exit(0) // need explicit exit for the app's utilizing NetServer/NetClient as they have actors
  }
}


class ScageScreen(unit_name: String = "Scage Screen") extends Screen(unit_name) with SingleController

class ScageScreenD(unit_name: String = "Scage Screen") extends ScreenD(unit_name) with SingleController

class ScageScreenMT(unit_name: String = "Scage Screen") extends Screen(unit_name) with ActorSingleController

class ScageScreenDMT(unit_name: String = "Scage Screen") extends ScreenD(unit_name) with ActorSingleController

class ScageScreenApp(title: String = property("app.name", "Scage App"),
                     width: Int = property("screen.width", 800),
                     height: Int = property("screen.height", 600)) extends ScreenApp(title, width, height) with SingleController

class ScageScreenAppD(title: String = property("app.name", "Scage App"),
                      width: Int = property("screen.width", 800),
                      height: Int = property("screen.height", 600)) extends ScreenAppD(title, width, height) with SingleController

class ScageScreenAppMT(title: String = property("app.name", "Scage App"),
                       width: Int = property("screen.width", 800),
                       height: Int = property("screen.height", 600)) extends ScreenAppMT(title, width, height) with ActorSingleController

class ScageScreenAppDMT(title: String = property("app.name", "Scage App"),
                        width: Int = property("screen.width", 800),
                        height: Int = property("screen.height", 600)) extends ScreenAppDMT(title, width, height) with ActorSingleController

abstract class ScageApplet extends Applet {
  def screen: ScageScreenApp

  /** The Canvas where the LWJGL Display is added */
  private var display_parent: Canvas = _

  /** Thread which runs the main game loop */
  private var gameThread: Thread = _

  /**
    * Once the Canvas is created its add notify method will call this method to
    * start the LWJGL Display and game loop in another thread.
    */
  private def startScage(): Unit = {
    gameThread = new Thread {
      override def run(): Unit = {
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
  private def stopScage(): Unit = {
    screen.stop()
    try {
      gameThread.join()
    } catch {
      case e: InterruptedException =>
        e.printStackTrace()
    }
  }

  /**
    * Applet Destroy method will remove the canvas, before canvas is destroyed it will notify
    * stopLWJGL() to stop main game loop and to destroy the Display
    */
  override def destroy(): Unit = {
    remove(display_parent)
    super.destroy()
  }

  /**
    * initialise applet by adding a canvas to it, this canvas will start the LWJGL Display and game loop
    * in another thread. It will also stop the game loop and destroy the display on canvas removal when
    * applet is destroyed.
    */
  override def init(): Unit = {
    setLayout(new BorderLayout())
    try {
      display_parent = new Canvas() {
        override def addNotify(): Unit = {
          super.addNotify()
          startScage()
        }

        override def removeNotify(): Unit = {
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
      case e: Exception =>
        System.err.println(e)
        throw new RuntimeException("Unable to create display")
    }
  }
}

abstract class ScageAppletD extends Applet {
  def screen: ScageScreenAppD

  /** The Canvas where the LWJGL Display is added */
  private var display_parent: Canvas = _

  /** Thread which runs the main game loop */
  private var gameThread: Thread = _

  /**
    * Once the Canvas is created its add notify method will call this method to
    * start the LWJGL Display and game loop in another thread.
    */
  private def startScage(): Unit = {
    gameThread = new Thread {
      override def run(): Unit = {
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
  private def stopScage(): Unit = {
    screen.stop()
    try {
      gameThread.join()
    } catch {
      case e: InterruptedException =>
        e.printStackTrace()
    }
  }

  /**
    * Applet Destroy method will remove the canvas, before canvas is destroyed it will notify
    * stopLWJGL() to stop main game loop and to destroy the Display
    */
  override def destroy(): Unit = {
    remove(display_parent)
    super.destroy()
  }

  /**
    * initialise applet by adding a canvas to it, this canvas will start the LWJGL Display and game loop
    * in another thread. It will also stop the game loop and destroy the display on canvas removal when
    * applet is destroyed.
    */
  override def init(): Unit = {
    setLayout(new BorderLayout())
    try {
      display_parent = new Canvas() {
        override def addNotify(): Unit = {
          super.addNotify()
          startScage()
        }

        override def removeNotify(): Unit = {
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
      case e: Exception =>
        System.err.println(e)
        throw new RuntimeException("Unable to create display")
    }
  }
}
