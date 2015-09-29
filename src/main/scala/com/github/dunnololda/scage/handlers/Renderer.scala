package com.github.dunnololda.scage.handlers

import java.awt.GraphicsEnvironment
import java.io.InputStream

import com.github.dunnololda.cli.AppProperties._
import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.handlers.controller2.ScageController
import com.github.dunnololda.scage.handlers.controller3.ControllerActorSystem
import com.github.dunnololda.scage.support.ScageColor._
import com.github.dunnololda.scage.support.ScageId._
import com.github.dunnololda.scage.support.messages.ScageXML._
import com.github.dunnololda.scage.support.messages.{ScageMessage, ScageXML}
import com.github.dunnololda.scage.support.tracer3.{ScageTracer, Trace}
import com.github.dunnololda.scage.support.{ScageColor, Vec}
import com.github.dunnololda.scage.{ScagePhase, Scage, ScageOperation}
import org.lwjgl.BufferUtils
import org.lwjgl.opengl.{Display, DisplayMode, GL11}
import org.lwjgl.util.glu.GLU
import org.newdawn.slick.opengl.{Texture, TextureLoader}
import org.newdawn.slick.util.ResourceLoader

import scala.collection.mutable.ArrayBuffer

object DisplayListsHolder {
  private val log = MySimpleLogger(this.getClass.getName)

  private val lists = ArrayBuffer[(Int, () => Unit)]()
  def addDisplayList(list_code:Int, func: => Unit) {
    lists += (list_code, () => func)
  }
  def reloadDisplayLists() {
    log.debug("reloading display lists...")
    for {
      (list_code, func) <- lists
    } {
      GL11.glNewList(list_code, GL11.GL_COMPILE)
      func()
      GL11.glEndList()
    }
  }
}

trait RendererLib {
  private val log = MySimpleLogger(this.getClass.getName)

  def backgroundColor = {
    val background_color = BufferUtils.createFloatBuffer(16)    
    GL11.glGetFloat(GL11.GL_COLOR_CLEAR_VALUE, background_color)
    new ScageColor(background_color.get(0), background_color.get(1), background_color.get(2))
  }
  def backgroundColor_=(c:ScageColor) {if(c != DEFAULT_COLOR) GL11.glClearColor(c.red, c.green, c.blue, 0)}

  def clearScreen() {
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT/* | GL11.GL_DEPTH_BUFFER_BIT*/)
    GL11.glLoadIdentity()
  }
  
  def currentColor = {
    val _color = BufferUtils.createFloatBuffer(16)
    GL11.glGetFloat(GL11.GL_CURRENT_COLOR, _color)
    new ScageColor(_color.get(0), _color.get(1), _color.get(2))
  }
  def currentColor_=(c:ScageColor) {if(c != DEFAULT_COLOR) GL11.glColor3f(c.red, c.green, c.blue)}

  def displayList(func: => Unit) = {
    val list_code = /*nextDisplayListKey*/nextId
    GL11.glNewList(list_code, GL11.GL_COMPILE)
    func
    GL11.glEndList()
    DisplayListsHolder.addDisplayList(list_code, func)
    list_code
  }

  def openglLocalTransform(transform: => Unit) {
    GL11.glPushMatrix()
    transform
    GL11.glPopMatrix()
  }

  def openglMove(vec:Vec) {GL11.glTranslatef(vec.x, vec.y, 0)}
  def openglMove(x:Float, y:Float) {GL11.glTranslatef(x, y, 0)}

  def openglRotate(ang_deg:Float) {GL11.glRotatef(ang_deg, 0, 0, 1)}
  def openglRotateDeg(ang_deg:Float) {GL11.glRotatef(ang_deg, 0, 0, 1)}
  def openglRotateRad(ang_rad:Float) {GL11.glRotatef((ang_rad*180f/math.Pi).toFloat, 0, 0, 1)}

  def openglScale(scale_factor:Float) {GL11.glScalef(scale_factor, scale_factor, 1)}

  private lazy val FILLED_CIRCLE = displayList {
    GL11.glBegin(GL11.GL_TRIANGLE_FAN)
    for(i <- 0 until 100) {
      val cosine = math.cos(i*2*math.Pi/100).toFloat
      val sine = math.sin(i*2*math.Pi/100).toFloat
      GL11.glVertex2f(cosine, sine)
    }
    GL11.glEnd()
  }

  private lazy val CIRCLE = displayList {
    GL11.glBegin(GL11.GL_LINE_LOOP)
      for(i <- 0 until 100) {
        val cosine = math.cos(i*2*math.Pi/100).toFloat
        val sine = math.sin(i*2*math.Pi/100).toFloat
        GL11.glVertex2f(cosine, sine)
      }
    GL11.glEnd()
  }
  def drawCircle(coord:Vec, radius:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glPushMatrix()
      GL11.glTranslatef(coord.x, coord.y, 0.0f)
      GL11.glScalef(radius,radius,1)
     	  GL11.glCallList(CIRCLE)
      GL11.glPopMatrix()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawEllipse(coord:Vec, radius1:Float, radius2:Float, color:ScageColor = DEFAULT_COLOR): Unit = {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glPushMatrix()
      GL11.glTranslatef(coord.x, coord.y, 0.0f)
      GL11.glScalef(radius1, radius2, 1)
        GL11.glCallList(CIRCLE)
      GL11.glPopMatrix()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawFilledCircle(coord:Vec, radius:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glPushMatrix()
      GL11.glTranslatef(coord.x, coord.y, 0.0f)
      GL11.glScalef(radius,radius,1)
     	  GL11.glCallList(FILLED_CIRCLE)
      GL11.glPopMatrix()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawFilledEllipse(coord:Vec, radius1:Float, radius2:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glPushMatrix()
      GL11.glTranslatef(coord.x, coord.y, 0.0f)
      GL11.glScalef(radius1,radius2,1)
      GL11.glCallList(FILLED_CIRCLE)
    GL11.glPopMatrix()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawLine(v1:Vec, v2:Vec, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    	GL11.glBegin(GL11.GL_LINES)
    		GL11.glVertex2f(v1.x, v1.y)
    		GL11.glVertex2f(v2.x, v2.y)
    	GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawLines(edges:Vec*) {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    	GL11.glBegin(GL11.GL_LINES)
    		edges.foreach(edge => GL11.glVertex2f(edge.x, edge.y))
    	GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawSlidingLines(edges:Vec*) {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glBegin(GL11.GL_LINES)
    edges.sliding(2).foreach {
      case Seq(a,b) =>
        GL11.glVertex2f(a.x, a.y)
        GL11.glVertex2f(b.x, b.y)
      case _ =>
    }
    GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def  drawSlidingLines(edges:Seq[Vec], color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glBegin(GL11.GL_LINES)
    edges.sliding(2).foreach {
      case Seq(a,b) =>
        GL11.glVertex2f(a.x, a.y)
        GL11.glVertex2f(b.x, b.y)
      case _ =>
    }
    GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawGroupedLines(edges:Vec*) {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glBegin(GL11.GL_LINES)
    val edges_ext = if(edges.length % 2 == 0) edges else edges.init
    edges_ext.grouped(2).foreach {
      case Seq(a,b) =>
        GL11.glVertex2f(a.x, a.y)
        GL11.glVertex2f(b.x, b.y)
      case _ =>
    }
    GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def  drawGroupedLines(edges:Seq[Vec], color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    GL11.glBegin(GL11.GL_LINES)
    val edges_ext = if(edges.length % 2 == 0) edges else edges.init
    edges_ext.grouped(2).foreach {
      case Seq(a,b) =>
        GL11.glVertex2f(a.x, a.y)
        GL11.glVertex2f(b.x, b.y)
      case _ =>
    }
    GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawLines(edges:Traversable[Vec], color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_LINES)
        edges.foreach(edge => GL11.glVertex2f(edge.x, edge.y))
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawRect(coord:Vec, width:Float, height:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glBegin(GL11.GL_LINE_LOOP)
          GL11.glVertex2f(coord.x, coord.y)
          GL11.glVertex2f(coord.x + width, coord.y)
          GL11.glVertex2f(coord.x + width, coord.y - height)
          GL11.glVertex2f(coord.x, coord.y - height)
        GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }
  def drawFilledRect(coord:Vec, width:Float, height:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glBegin(GL11.GL_QUADS)
        GL11.glVertex2f(coord.x, coord.y)
        GL11.glVertex2f(coord.x + width, coord.y)
        GL11.glVertex2f(coord.x + width, coord.y - height)
        GL11.glVertex2f(coord.x, coord.y - height)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }
  def drawRectCentered(coord:Vec, width:Float, height:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glBegin(GL11.GL_LINE_LOOP)
          GL11.glVertex2f(coord.x - width/2, coord.y - height/2)
          GL11.glVertex2f(coord.x - width/2, coord.y + height/2)
          GL11.glVertex2f(coord.x + width/2, coord.y + height/2)
          GL11.glVertex2f(coord.x + width/2, coord.y - height/2)
        GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }
  def drawFilledRectCentered(coord:Vec, width:Float, height:Float, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glBegin(GL11.GL_QUADS)
        GL11.glVertex2f(coord.x - width/2, coord.y - height/2)
        GL11.glVertex2f(coord.x - width/2, coord.y + height/2)
        GL11.glVertex2f(coord.x + width/2, coord.y + height/2)
        GL11.glVertex2f(coord.x + width/2, coord.y - height/2)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawPolygon(coords:Vec*) {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_LINE_LOOP)
        for(coord <- coords) GL11.glVertex2f(coord.x, coord.y)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawPolygon(coords:Traversable[Vec], color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_LINE_LOOP)
        for(coord <- coords) GL11.glVertex2f(coord.x, coord.y)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawFilledPolygon(coords:Vec*) {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      /*GL11.glPolygonMode(GL11.GL_FRONT, GL11.GL_FILL)
      GL11.glPolygonMode(GL11.GL_BACK, GL11.GL_LINE)*/
      GL11.glBegin(GL11.GL_POLYGON)
        for(coord <- coords) GL11.glVertex2f(coord.x, coord.y)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawFilledPolygon(coords:Traversable[Vec], color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
    /*GL11.glPolygonMode(GL11.GL_FRONT, GL11.GL_FILL)
    GL11.glPolygonMode(GL11.GL_BACK, GL11.GL_LINE)*/
      GL11.glBegin(GL11.GL_POLYGON)
        for(coord <- coords) GL11.glVertex2f(coord.x, coord.y)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawPoint(coord:Vec, color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_POINTS)
        GL11.glVertex2f(coord.x, coord.y)
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawPoints(coords:Vec*) {
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_POINTS)
        coords.foreach(coord => GL11.glVertex2f(coord.x, coord.y))
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  def drawPoints(coords:Traversable[Vec], color:ScageColor = DEFAULT_COLOR) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glDisable(GL11.GL_TEXTURE_2D)
      GL11.glBegin(GL11.GL_POINTS)
        coords.foreach(coord => GL11.glVertex2f(coord.x, coord.y))
      GL11.glEnd()
    GL11.glEnable(GL11.GL_TEXTURE_2D)
  }

  // white color by default for display lists to draw in natural colors
  def drawDisplayList(list_code:Int, coord:Vec = Vec.zero, color:ScageColor = WHITE) {
    if(color != DEFAULT_COLOR) currentColor = color
    GL11.glPushMatrix()
	  GL11.glTranslatef(coord.x, coord.y, 0.0f)
	  GL11.glCallList(list_code)
	  GL11.glPopMatrix()
  }

  def drawTraceGrid(tracer:ScageTracer[_], color:ScageColor = DEFAULT_COLOR) {
    drawLines(tracer.trace_grid, color)
  }

  def drawTraceLocations[T <: Trace](tracer:ScageTracer[T], color:ScageColor = DEFAULT_COLOR, radius:Int = 3) {
    tracer.tracesList.foreach(trace => drawFilledCircle(trace.location, radius, color))
  }

  def getTexture(format:String, in:InputStream):Texture = TextureLoader.getTexture(format, in)
  def getTexture(filename:String):Texture = {
    val format:String = filename.substring(filename.length-3)
    getTexture(format, ResourceLoader.getResourceAsStream(filename))    // can be loaded as resource from jar, hell yeah!!
  }

  def image(texture:Texture, game_width:Float, game_height:Float, start_x:Float, start_y:Float, real_width:Float, real_height:Float):Int = {
	  val list_name = /*nextDisplayListKey*/nextId

		val t_width:Float = texture.getTextureWidth
		val t_height:Float = texture.getTextureHeight

		GL11.glNewList(list_name, GL11.GL_COMPILE)
		//texture.bind
	  	GL11.glBindTexture(GL11.GL_TEXTURE_2D, texture.getTextureID)
		GL11.glBegin(GL11.GL_QUADS)
			GL11.glTexCoord2f(start_x/t_width, start_y/t_height)
	    GL11.glVertex2f(-game_width/2, game_height/2)

			GL11.glTexCoord2f((start_x+real_width)/t_width, start_y/t_height)
			GL11.glVertex2f(game_width/2, game_height/2)

			GL11.glTexCoord2f((start_x+real_width)/t_width, (start_y+real_height)/t_height)
			GL11.glVertex2f(game_width/2, -game_height/2)

	    GL11.glTexCoord2f(start_x/t_width, (start_y+real_height)/t_height)
			GL11.glVertex2f(-game_width/2, -game_height/2)
		GL11.glEnd()
		GL11.glEndList()

		list_name
	}

  val images_base = property("images.base", "resources/images/")
  def image(filename:String, game_width:Float, game_height:Float, start_x:Float, start_y:Float, real_width:Float, real_height:Float):Int = {
    image(getTexture(images_base+filename), game_width, game_height, start_x, start_y, real_width, real_height)
  }

  // TODO: is Array[Int] preferable or IndexedSeq[Int] is fine??
  //private var animations = new HashMap[Int, (IndexedSeq[Int], Int)]
  def animation(filename:String, game_width:Float, game_height:Float, real_width:Float, real_height:Float, num_frames:Int) = {
    val texture = getTexture(images_base+filename)
    val columns:Int = (texture.getImageWidth/real_width).toInt
    /*val frames = */for {
      frame <- 0 until num_frames
      x = real_width*(frame - frame/columns*columns)
      y = real_height*(frame/columns)
    } yield image(texture, game_width, game_height, x, y, real_width, real_height)
    /*val animation_id = nextId
    animations += (animation_id -> (frames, 0))
    animation_id*/
  }

  /*def drawAnimation(animation_id:Int, coord:Vec) {
    if(animations.contains(animation_id)) {
      val (frames, current_frame) = animations(animation_id)
      drawDisplayList(frames(current_frame), coord)
      val next_frame = if(current_frame >= frames.length-1) 0 else current_frame + 1
      animations(animation_id) = (frames, next_frame)
    }
  }*/
  
  def windowSize = {
    Vec(Display.getDisplayMode.getWidth, Display.getDisplayMode.getHeight)
  }

  def windowSize_=(new_window_resolution:(Int, Int)) {
    val (new_window_width, new_window_height) = new_window_resolution
    if(new_window_width != windowWidth || new_window_height != windowHeight) {
      log.debug("changing resolution to "+new_window_width+"x"+new_window_height+"...")
      val backup_background_color = backgroundColor
      val backup_current_color = currentColor
      destroygl()
      initgl(new_window_width, new_window_height)
      ScageMessage.reloadFont()
      DisplayListsHolder.reloadDisplayLists()
      currentColor = backup_current_color
      backgroundColor = backup_background_color
    }
  }

  def windowSizeMT = Vec(Display.getDisplayMode.getWidth, Display.getDisplayMode.getHeight)

  def windowSizeMT_=(new_window_resolution:(Int, Int)) {
    val (new_window_width, new_window_height) = new_window_resolution
    if(new_window_width != RendererLib.windowWidth || new_window_height != RendererLib.windowHeight) {
      log.debug("changing resolution to "+new_window_width+"x"+new_window_height+"...")
      val backup_background_color = RendererLib.backgroundColor
      val backup_current_color = RendererLib.currentColor
      val backup_controlled_keys = ControllerActorSystem.monitoredKeysList
      ControllerActorSystem.stopCheckControls()
      ControllerActorSystem.shutdownControllerActor()
      Display.destroy()
      ControllerActorSystem.createControllerActor()
      ControllerActorSystem.initGLAndReleaseContext(new_window_width, new_window_height, RendererLib.windowTitle)
      Display.makeCurrent()
      ScageMessage.reloadFont()
      DisplayListsHolder.reloadDisplayLists()
      ControllerActorSystem.addKeys(backup_controlled_keys)
      ControllerActorSystem.startCheckControls()
      RendererLib.currentColor = backup_current_color
      RendererLib.backgroundColor = backup_background_color
    }
  }

  def windowWidth = Display.getDisplayMode.getWidth
  def windowWidth_=(new_window_width:Int) {
    windowSize = (new_window_width, windowHeight)
  }

  def windowHeight = Display.getDisplayMode.getHeight
  def windowHeight_=(new_window_height:Int) {
    windowSize = (windowWidth, new_window_height)
  }

  def windowTitle = Display.getTitle
  def windowTitle_=(new_title:String = windowTitle) {
    Display.setTitle(new_title)
  }

  private[scage] def initgl(window_width:Int = windowWidth, window_height:Int = windowHeight, title:String = windowTitle) {
    Display.setDisplayMode(new DisplayMode(window_width, window_height))
    Display.setVSyncEnabled(property("render.vsync", false))
    Display.setTitle(title)
    Display.create()

    val center_point = GraphicsEnvironment.getLocalGraphicsEnvironment.getCenterPoint
    Display.setLocation(center_point.getX.toInt - window_width/2, center_point.getY.toInt - window_height/2)

    GL11.glEnable(GL11.GL_TEXTURE_2D)
    GL11.glClearColor(0,0,0,0)
    GL11.glDisable(GL11.GL_DEPTH_TEST)

    GL11.glEnable(GL11.GL_BLEND)
    GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

    GL11.glMatrixMode(GL11.GL_PROJECTION) // Select The Projection Matrix
    GL11.glLoadIdentity() // Reset The Projection Matrix
    GLU.gluOrtho2D(0, window_width, 0, window_height)
    //GL11.glOrtho(0, width, height, 0, 1, -1)

    GL11.glMatrixMode(GL11.GL_MODELVIEW)
    GL11.glLoadIdentity()

    // printing "Loading..." message. It is also necessary to properly initialize our main font (I guess)
    GL11.glClear(GL11.GL_COLOR_BUFFER_BIT/* | GL11.GL_DEPTH_BUFFER_BIT*/)

    log.info("initialized opengl system")
  }

  private[scage] def drawWelcomeMessages() {
    ScageMessage.print(xmlOrDefault("renderer.loading", "Loading..."), 20, windowHeight-25, GREEN)
    Display.update()
    Thread.sleep(1000)

    // drawing scage logo
    if(property("screen.scagelogo", true)) {  // TODO: replace this with two different builds (under different maven profiles): with and without scagelogo
      GL11.glClear(GL11.GL_COLOR_BUFFER_BIT/* | GL11.GL_DEPTH_BUFFER_BIT*/)
      val logo_texture = getTexture("resources/images/scage-logo.png")
      drawDisplayList(image(logo_texture, windowWidth, windowHeight, 0, 0, logo_texture.getImageWidth, logo_texture.getImageHeight), Vec(windowWidth/2, windowHeight/2))
      Display.update()
      Thread.sleep(1000)
    }

    // drawing app logo or welcome message
    stringProperty("screen.splash") match { // TODO: change this to custom init sequence (any graphical routines here, default is the one splash screen with logo)
      case screen_splash_path if "" != screen_splash_path =>
        try {
          GL11.glClear(GL11.GL_COLOR_BUFFER_BIT/* | GL11.GL_DEPTH_BUFFER_BIT*/)
          val splash_texture = getTexture(screen_splash_path)
          drawDisplayList(image(splash_texture, windowWidth, windowHeight, 0, 0, splash_texture.getImageWidth, splash_texture.getImageHeight), Vec(windowWidth/2, windowHeight/2))
          Display.update()
          Thread.sleep(1000)  // TODO: custom pause
        }
        catch {
          case e:Exception => log.error("failed to render splash image: "+e.getLocalizedMessage)
        }
      case _ => xmlOrDefault("app.welcome", "") match {
        case welcome_message if "" != welcome_message =>
          GL11.glClear(GL11.GL_COLOR_BUFFER_BIT/* | GL11.GL_DEPTH_BUFFER_BIT*/)
          print(welcome_message, 20, windowHeight-25, GREEN) // TODO: custom color and position
          Display.update()
          Thread.sleep(1000) // TODO: custom pause
        case _ =>
      }
    }
  }

  private[scage] def renderExitMessage() {
    backgroundColor = BLACK
    clearScreen()
    ScageMessage.print(xmlOrDefault("renderer.exiting", "Exiting..."), 20, windowHeight-25, GREEN)
    Display.update()
    Thread.sleep(1000)
  }

  private[scage] def destroygl() {
    Display.destroy()
    log.info("destroyed opengl system")
  }
}

object RendererLib extends RendererLib
import com.github.dunnololda.scage.handlers.RendererLib._

trait Renderer extends Scage with ScageController {
  //private val log = MySimpleLogger(this.getClass.getName)

  private var _fps:Int = 0
  def fps = _fps

  private var _ticks:Int = 0
  def ticks = {
    if(_ticks != 0) {
      _ticks
    } else {
      TICKS_PER_SECOND
    }
  }

  private var _action_time_msec:Long = 0l
  private var _average_action_time_msec:Float = 0f
  private var _action_time_measures_count:Long = 0l
  def currentActionTimeMsec = _action_time_msec
  def averageActionTimeMsec = _average_action_time_msec

  private var _render_time_msec:Long = 0l
  private var _average_render_time_msec:Float = 0f
  private var _render_time_measures_count:Long = 0l
  def currentRenderTimeMsec = _render_time_msec
  def averageRenderTimeMsec = _average_render_time_msec

  private var msec = System.currentTimeMillis
  private var frames:Int = 0
  private var msec4 = System.currentTimeMillis
  private def countFPS() {
    frames += 1
    if(System.currentTimeMillis - msec >= 1000) {
      _fps = frames
      frames = 0
      msec = System.currentTimeMillis
    }
  }

  private var msec2 = System.currentTimeMillis
  private var frames2:Int = 0
  private var msec3 = System.currentTimeMillis
  private def countTicks() {
    frames2 += 1
    if(System.currentTimeMillis - msec2 >= 1000) {
      _ticks = frames2
      frames2 = 0
      msec2 = System.currentTimeMillis
    }
  }

  private var next_game_tick_diff_save = 0l
  private var msec_diff_save = 0l
  private var msec2_diff_save = 0l
  private var msec3_diff_save = 0l
  private var msec4_diff_save = 0l
  def saveCounters() {
    next_game_tick_diff_save = System.currentTimeMillis() - next_game_tick
    msec_diff_save = System.currentTimeMillis() - msec_diff_save
    msec2_diff_save = System.currentTimeMillis() - msec2_diff_save
    msec3_diff_save = System.currentTimeMillis() - msec3_diff_save
    msec4_diff_save = System.currentTimeMillis() - msec4_diff_save
  }
  def restoreCounters() {
    next_game_tick = next_game_tick + next_game_tick_diff_save
    next_game_tick_diff_save = 0l

    msec = msec + msec_diff_save
    msec_diff_save = 0l

    msec2 = msec2 + msec2_diff_save
    msec2_diff_save = 0l

    msec3 = msec3 + msec3_diff_save
    msec3_diff_save = 0l

    msec4 = msec4 + msec4_diff_save
    msec4_diff_save = 0l
  }
  def holdCounters(func: => Any) {
    saveCounters()
    func
    restoreCounters()
  }

  def resetCounters() {
    next_game_tick = System.currentTimeMillis()
    msec = System.currentTimeMillis()
    frames = 0
    msec2 = System.currentTimeMillis()
    frames2 = 0
    msec3 = System.currentTimeMillis()
    msec4 = System.currentTimeMillis()
  }

  private var _base:() => Vec = () => Vec.zero
  def base = _base()
  def base_= (coord: => Vec) {_base = () => coord}

  private var _global_scale:Float = 1.0f
  def globalScale = _global_scale
  def globalScale_= (new_global_scale:Float) {_global_scale = new_global_scale}

  private var window_center:() => Vec = () => /*RendererInitializer.default_window_center*//*Vec(windowWidth/2, windowHeight/2)*/ windowSize/2
  def windowCenter:Vec = window_center()
  def windowCenter_= (coord: => Vec) {window_center = () => coord}
  
  private var central_coord:() => Vec = window_center
  def center = central_coord()
  def center_= (coord: => Vec) {central_coord = () => coord}

  private var rotation_point:() => Vec = () => Vec.zero
  private var rotation_angle:() => Float = () => 0f // in degrees
  def rotation = (rotation_point(), rotation_angle())
  def rotation_=(new_rotation_params: => (Vec, Float)) {
    rotation_point = () => new_rotation_params._1
    rotation_angle = () => new_rotation_params._2
  }
  @deprecated("use rotationCenter", "2.0") def rotationPoint = rotation_point()
  @deprecated("use rotationCenter", "2.0") def rotationPoint_=(new_rotation_point: => Vec) {
    rotation_point = () => new_rotation_point
  }
  def rotationCenter = rotation_point()
  def rotationCenter_=(new_rotation_point: => Vec) {
    rotation_point = () => new_rotation_point
  }
  def rotationDeg = (rotation_point(), rotation_angle())
  def rotationDeg_=(new_rotation_params: => (Vec, Float)) {
    rotation_point = () => new_rotation_params._1
    rotation_angle = () => new_rotation_params._2
  }
  def rotationRad = (rotation_point(), rotation_angle()/180f*math.Pi.toFloat)
  def rotationRad_=(new_rotation_params: => (Vec, Float)) {
    rotation_point = () => new_rotation_params._1
    rotation_angle = () => new_rotation_params._2*180/math.Pi.toFloat
  }
  def rotationAngle = rotation_angle()
  def rotationAngle_=(new_rotation_angle: => Float) {
    rotation_angle = () => new_rotation_angle
  }
  def rotationAngleDeg = rotation_angle()
  def rotationAngleDeg_=(new_rotation_angle: => Float) {
    rotation_angle = () => new_rotation_angle
  }
  def rotationAngleRad = rotation_angle()/180f*math.Pi.toFloat
  def rotationAngleRad_=(new_rotation_angle_rad: => Float) {
    rotation_angle = () => new_rotation_angle_rad*180/math.Pi.toFloat
  }

  @deprecated("use absCoord", "2.0") def scaledCoord(coord:Vec) = {
    (coord / globalScale) + (center - windowCenter/globalScale)
  }

  def absCoord(coord:Vec) = {
    val x = (coord / globalScale) + (center - windowCenter/globalScale)
    val a = rotation_angle()
    if(a != 0) {
      val p = rotation_point()
      (x - p).rotateDeg(-a) + p
    } else {
      x
    }
  }

  private[scage] val renders = defaultContainer("renders", ScagePhase.Render, execute_if_app_running = false, execute_on_deletion = false)

  def render(render_func: => Any) = renders.addOp(() => render_func, 0)
  def render(position:Int)(render_func: => Any) = renders.addOp(() => render_func, position)

  def delRender(operation_id:Int) = {renders.delOperation(operation_id)}
  def delRenders(operation_ids:Int*) {renders.delOperations(operation_ids:_*)}
  def delAllRenders() {renders.delAllOperations()}
  def delAllRendersExcept(except_operation_ids:Int*) {renders.delAllOperationsExcept(except_operation_ids:_*)}

  private[scage] val interfaces = defaultContainer("interfaces", ScagePhase.Interface, execute_if_app_running = false, execute_on_deletion = false)

  def interface(interface_func: => Any):Int = {
    interfaces.addOp(() => interface_func, 0)
  }

  def interface(position:Int)(interface_func: => Any):Int = {
    interfaces.addOp(() => interface_func, position)
  }

  def interfaceFromXml(interface_id:String, parameters: => Array[Any] = Array[Any]()):Int = {
    interface {
      ScageMessage.printInterface(ScageXML.xmlInterface(interface_id, parameters:_*))
    }
  }

  def delInterface(operation_id:Int) = {interfaces.delOperation(operation_id)}
  def delInterfaces(operation_ids:Int*) {interfaces.delOperations(operation_ids:_*)}
  def delAllInterfaces() {interfaces.delAllOperations()}
  def delAllInterfacesExcept(except_operation_ids:Int*) {interfaces.delAllOperationsExcept(except_operation_ids:_*)}

  val TICKS_PER_SECOND = property("render.ticks", 60)
  val SKIP_TICKS = 1000 / TICKS_PER_SECOND
  val MAX_FRAMESKIP = property("render.frameskip", 5)
  private var loops = 0

  private var next_game_tick = System.currentTimeMillis()
  private[scage] def prepareRendering() {
    next_game_tick = System.currentTimeMillis()
  }

  private var _interpolation:Float = 0
  def interpolation = _interpolation

  /*private var actions_run_count = 0
  private var actions_run_moment = System.currentTimeMillis()*/

  private def _execute(_actions_iterator:Iterator[ScageOperation]) {
    scage_phase = ScagePhase.Action
    while(is_running && Scage.isAppRunning && !restart_toggled && _actions_iterator.hasNext) {
      val ScageOperation(action_id, action_operation, _) = _actions_iterator.next()
      current_operation_id = action_id
      action_operation()
    }
    executeDelAndAddOperationsIfExist()
    scage_phase = ScagePhase.NoPhase
  }
  private[scage] def checkControlsAndExecuteActions() {  // maybe rename it to not confuse clients
    if(System.currentTimeMillis() - next_game_tick > 60000) {
      resetCounters()
    }
    loops = 0
    while(is_running && Scage.isAppRunning && System.currentTimeMillis() > next_game_tick && loops < MAX_FRAMESKIP) {
      scage_phase = ScagePhase.Controls
      checkControls()
      executeDelAndAddOperationsIfExist()
      scage_phase = ScagePhase.NoPhase
      restart_toggled = false
      msec3 = System.currentTimeMillis()
      if(is_running && Scage.isAppRunning && actions.operations.nonEmpty) {
        _execute(actions.operations.iterator)
      }
      _action_time_msec = System.currentTimeMillis() - msec3
      _action_time_measures_count += 1
      _average_action_time_msec =1f*(_average_action_time_msec*(_action_time_measures_count-1) + _action_time_msec)/_action_time_measures_count
      countTicks()
      next_game_tick += SKIP_TICKS
      loops += 1
    }
    _interpolation = (System.currentTimeMillis() + SKIP_TICKS - next_game_tick).toFloat/SKIP_TICKS
  }

  val framerate = property("render.framerate", 0)

  private def executeRenders(_renders_iterator:Iterator[ScageOperation]) {
    scage_phase = ScagePhase.Render
    while(is_running && Scage.isAppRunning && !restart_toggled && _renders_iterator.hasNext) {
      val ScageOperation(render_id, render_operation, _) = _renders_iterator.next()
      current_operation_id = render_id
      GL11.glPushMatrix()
      render_operation()
      GL11.glPopMatrix()
    }
    executeDelAndAddOperationsIfExist()
    scage_phase = ScagePhase.NoPhase
  }
  private def executeInterfaces(_interfaces_iterator:Iterator[ScageOperation]) {
    scage_phase = ScagePhase.Interface
    while(is_running && Scage.isAppRunning && !restart_toggled && _interfaces_iterator.hasNext) {
      val ScageOperation(interface_id, interface_operation, _) = _interfaces_iterator.next()
      current_operation_id = interface_id
      interface_operation()
    }
    executeDelAndAddOperationsIfExist()
    scage_phase = ScagePhase.NoPhase
  }
  private[scage] def performRendering() {
    if (Display.isCloseRequested) Scage.stopApp()
    else {
      msec4 = System.currentTimeMillis()
      clearScreen()
      GL11.glPushMatrix()
      val coord = window_center() - (central_coord() - _base()) * _global_scale
      if (coord.notZero) GL11.glTranslatef(coord.x, coord.y, 0.0f)
      if (_global_scale != 1) GL11.glScalef(_global_scale, _global_scale, 1)
      val rot_ang = rotation_angle()
      if (rot_ang != 0) {
        val point = rotation_point() - _base()
        GL11.glTranslatef(point.x, point.y, 0.0f)
        GL11.glRotatef(rot_ang, 0, 0, 1)
        GL11.glTranslatef(-point.x, -point.y, 0.0f)
      }
      if (is_running && Scage.isAppRunning && renders.operations.nonEmpty) {
        executeRenders(renders.operations.iterator)
      }
      GL11.glPopMatrix()

      if (is_running && Scage.isAppRunning && interfaces.operations.nonEmpty) {
        executeInterfaces(interfaces.operations.iterator)
      }

      if (framerate != 0) Display.sync(framerate)

      Display.update()
      _render_time_msec = System.currentTimeMillis() - msec4
      _render_time_measures_count += 1
      _average_render_time_msec = 1f * (_average_render_time_msec * (_render_time_measures_count - 1) + _render_time_msec) / _render_time_measures_count
      countFPS()
    }
  }
}
