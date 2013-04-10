package net.scage.test

import net.scage.ScageLib._
import net.scage.support.messages.ScageMessage
import concurrent.ops._
import junit.framework._
import Assert._
import net.scage.support.physics.ScagePhysics
import net.scage.support.tracer3.{Trace, CoordTracer}
import net.scage.support.physics.objects.{StaticPolygon, DynaBall}
import collection.mutable.ListBuffer
import javax.swing.JOptionPane
import net.scage.handlers.controller2.MultiController
import net.scage.ScreenApp
import net.scage.support.Vec
import net.scage.support.State
import net.scage.support.net.{NetClient, NetServer}

object ScageTest {
    def suite: Test = {
        val suite = new TestSuite(classOf[ScageTest])
        suite
    }

    def main(args : Array[String]) {
        junit.textui.TestRunner.run(suite)
    }
}

/**
 * Unit test for simple App.
 */
class ScageTest extends TestCase("app") {

    /**
     * Rigourous Tests :-)
     */
    def testOK() {
      new ScreenApp("Hello World") with MultiController {
        windowTitle += " - "+app_version
        /*scage_log.info("starting main unit "+unit_name+"...")
        ScageProperties.properties = properties*/

        val rect_color = property("rect.color", RED)
        val tracer = CoordTracer()

        val trace = tracer.addTrace(Vec(windowWidth/2, windowHeight/2))
        render(10) {
          drawTraceLocations(tracer, GREEN, 1)
        }

        val another_trace = tracer.addTrace(Vec(windowWidth/4, windowHeight/2))

        def moveIfFreeLocation(trace:Trace, delta:Vec) {
          val new_location = trace.location + delta
          if(!tracer.hasCollisions(trace.id, new_location, 20))    // test collisions using tracer
            tracer.updateLocation(trace.id, new_location)
        }

        anykey(onKeyDown = println("any key pressed =)"))   // test special method to obtain "press any key" event

        private var wanna_go_up = false
        key(KEY_W, onKeyDown = wanna_go_up = true, onKeyUp = wanna_go_up = false)
        private var wanna_go_left = false
        key(KEY_A, onKeyDown = wanna_go_left = true, onKeyUp = wanna_go_left = false)
        private var wanna_go_down = false
        key(KEY_S, onKeyDown = wanna_go_down = true, onKeyUp = wanna_go_down = false)
        private var wanna_go_right = false
        key(KEY_D, onKeyDown = wanna_go_right = true, onKeyUp = wanna_go_right = false)

        var dir = Vec.zero
        action {
          dir = Vec.zero
          if(wanna_go_up) dir += Vec(0, 1)
          if(wanna_go_down) dir += Vec(0, -1)
          if(wanna_go_right) dir += Vec(1, 0)
          if(wanna_go_left) dir += Vec(-1, 0)
          moveIfFreeLocation(trace, dir.n)
        }

        key(KEY_W, onKeyDown = println("also, W was pressed :3"))   // test for multiple functions on one key

        private var input_text = ""
        key(KEY_F1, onKeyDown = spawn {
          input_text = JOptionPane.showInputDialog("Input text here")
        })

        /*leftMouse(onBtnDown = {
          mouse_coord => tracer.updateLocation(trace, mouse_coord)
        })*/

        val physics = new ScagePhysics
        val poly_physical = physics.addPhysical(new StaticPolygon(Vec(100, 300), Vec(150, 250), Vec(300, 300), Vec(300, 450), Vec(200, 400)))
        val poly_render = displayList {   // test special method to save any opengl code as display list
          drawPolygon(poly_physical.points, CYAN)
        }

        val stars = displayList {   // I like "starry sky" since high school =)
          for(i <- 1 to 100) {
            drawPoint(Vec(math.random.toFloat*windowWidth, math.random.toFloat*windowHeight), randomColor)
          }
        }

        private var target_point = trace.location
        mouseMotion {   // test mouse motion event
          mouse_coord =>
            target_point = (scaledCoord(mouse_coord) - trace.location).n * 20
        }
        private var x = 0.0f
        def period = {
          x += 0.01f
          if(x > 2*math.Pi) x = 0
          (125 * 0.25f*(math.sin(x)) + 1).toLong
        }
        actionDynamicPeriod(period) {  // test actions with non-static period defined as function
          physics.step()
        }
        leftMouse(onBtnDown = {  // test left mouse event
          mouse_coord => physics.addPhysical(new DynaBall(trace.location + target_point, 2) {
            val ball_trace = tracer.addTrace(trace.location + target_point)
            val action_id:Int = action {
              tracer.updateLocation(ball_trace.id, coord)
              coord = ball_trace.location
              /*if(!tracer.isCoordOnArea(coord)) {
                isActive = false
                delActionOperation(action_id)
              }*/
            }

            velocity = (target_point).n*10
            render {
              /*if(physics.containsPhysical(this)) */drawFilledCircle(coord, 2, YELLOW)
            }
          })
        })

        backgroundColor = fromStringOrDefault("BLACK", BLACK)    // test method to obtain color by name
        val another_font = new ScageMessage(max_font_size = 15, font_file = "comic.ttf") // test using two different fonts in one app
        interface {
          another_font.print(xml("hello.world"), windowWidth/2, windowHeight/2+20, WHITE)
        }

        interfaceFromXml("scagetest.help", Array(trace.location, tracer.point(trace.location), fps, input_text))

        private var touches:ListBuffer[(Vec, Long)] = ListBuffer()    // test obtaining touching points for physical objects
        action {
          for {
            (point, _) <- poly_physical.touchingPoints
            if !touches.exists(_._1 == point)
          } touches += ((point, System.currentTimeMillis))

          for {
            t <- touches
            if System.currentTimeMillis - t._2 > 5000
          } touches -= t
        }

        render {
          def pew(v:Vec) {
            drawFilledRect(Vec(30, 30)+v, 60, 20, rect_color)
            drawDisplayList(stars,v)
            drawFilledCircle(trace.location+v, 10, RED)
            drawLine(trace.location+v, trace.location+v + target_point+v)
            drawCircle(another_trace.location+v, 10, GREEN)
            drawDisplayList(poly_render, v)
            //physics.physicals.foreach(p => drawFilledCircle(p.coord+v, 2, YELLOW))
            for((point, _) <- touches) drawFilledCircle(point+v, 3, RED)
          }
          pew(Vec.zero)
          if(globalScale > 1) {
            pew(Vec(0, windowHeight - 40))
            pew(Vec(0, -windowHeight + 40))
            pew(Vec(windowWidth - 80, 0))
            pew(Vec(-windowWidth + 80, 0))

            pew(Vec(windowWidth - 80, windowHeight - 40))
            pew(Vec(windowWidth - 80, -windowHeight + 40))
            pew(Vec(-windowWidth + 80, windowHeight - 40))
            pew(Vec(-windowWidth + 80, -windowHeight + 40))
          }
        }
        render(10) {
          def pew(v:Vec) {drawFilledRect(Vec(100, 30)+v, 60, 20, YELLOW)}
          pew(Vec.zero)
          if(globalScale > 1) {
            pew(Vec(0, windowHeight - 40))
            pew(Vec(0, -windowHeight + 40))
            pew(Vec(windowWidth - 80, 0))
            pew(Vec(-windowWidth + 80, 0))

            pew(Vec(windowWidth - 80, windowHeight - 40))
            pew(Vec(windowWidth - 80, -windowHeight + 40))
            pew(Vec(-windowWidth + 80, windowHeight - 40))
            pew(Vec(-windowWidth + 80, -windowHeight + 40))
          }
        }
        render(-10) {
          def pew(v:Vec) {drawLines(tracer.trace_grid.map(_ + v), DARK_GRAY)}
          pew(Vec.zero)
          if(globalScale > 1) {
            pew(Vec(0, windowHeight - 40))
            pew(Vec(0, -windowHeight + 40))
            pew(Vec(windowWidth - 80, 0))
            pew(Vec(-windowWidth + 80, 0))

            pew(Vec(windowWidth - 80, windowHeight - 40))
            pew(Vec(windowWidth - 80, -windowHeight + 40))
            pew(Vec(-windowWidth + 80, windowHeight - 40))
            pew(Vec(-windowWidth + 80, -windowHeight + 40))
          }
        }
        
        // scaling test
        mouseWheelUp(onWheelUp = m => globalScale += 1)
        mouseWheelDown(onWheelDown = m => if(globalScale > 1) globalScale -= 1)
        center = if(globalScale > 1) trace.location else windowCenter

        // test network features: server and client in one app. Client sends 2d vectors to server and server sends back normalized vector
        /*NetServer.startServer(
          onNewConnection = {
            client => client.send(State(("hello" -> "send me vec and I send you back its n!")))
            (true, "")
          },
          onClientDataReceived = {
            (client, received_data) => received_data.neededKeys {
              case ("vec", vec:Vec) => {
                client.send(State(("n" -> vec.n)))
              }
            }
          },
          onClientQuestion = {(client, question) =>
            val answer = State()
            question.neededKeys {
              case ("Are u ready?", true) => answer.add("yes")
            }
            answer
          }
        )

        action {
          if(NetServer.connectionPort != 0) {
            NetClient.startClient(
              server_url = "localhost",
              port = NetServer.connectionPort,
              onServerDataReceived = {
                received_data => received_data.neededKeys {
                  case ("hello", hello_msg) =>
                    val random_vec = Vec((math.random*100).toInt, (math.random*100).toInt)
                    NetClient.send(State(("vec" -> random_vec)))
                  case ("n", n:Vec) =>
                    println("received n: "+n)
                    println("waiting 5 sec...")
                    Thread.sleep(5000)
                    println("asking if the server is ready to receive another vec")
                    val answer = NetClient.askServer(State("Are u ready?"))
                    if(answer.contains("yes")) {
                      val random_vec = Vec((math.random*100).toInt, (math.random*100).toInt)
                      NetClient.send(State(("vec" -> random_vec)))
                    }
                }
              }
            )
            deleteSelf()
          }
        }

        dispose {
          NetServer.stopServer()
          NetClient.stopClient()
        }*/

        render {
          openglMove(windowCenter)
          openglRotate((msecsFromInit % 3600).toFloat/10)
          drawFilledPolygon(Array(Vec(-20, -5), Vec(20, -5), Vec(0, 20)), GREEN)
        }

        // window button test
        val window_button_area = List(windowCenter + windowCenter/2 + Vec(-40, -40),
                                      windowCenter + windowCenter/2 + Vec(40, -40),
                                      windowCenter + windowCenter/2 + Vec(0, 40))
        val window_button = leftMouseOnArea(area = window_button_area,
          onBtnDown = m => {println("window button pressed")},
          onBtnUp   = m => {println("window button released")})
        render {
          val window_button_pressed = leftMousePressed && mouseOnArea(window_button_area)
          currentColor = if(window_button_pressed) RED else WHITE
          drawPolygon(window_button_area)
          if(!window_button_pressed) print("Press Me", windowCenter + windowCenter/2, align = "center")
          else print("Release Me", windowCenter + windowCenter/2, align = "center")
        }

      }.main(Array[String]())
      assertTrue(true)
    }
}

/*test*/
