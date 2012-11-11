package net.scage.support.net

import _root_.net.scage.support.ScageProperties._
import java.io.{InputStreamReader, OutputStreamWriter, BufferedReader, PrintWriter}
import java.net.Socket
import net.scage.support.State
import com.weiglewilczek.slf4s.Logger
import actors.Actor._
import actors.Actor
import concurrent.SyncVar

object NetClient extends NetClient

class NetClient {
  private val log = Logger(this.getClass.getName)

  private lazy val io_actor:Actor = actor {
    var is_connected = false

    var socket:Socket = null
    var out:PrintWriter = null
    var in:BufferedReader = null

    var server_url:String = ""
    var port:Int = 0
    var ping_timeout:Int = 60000
    var onServerDataReceived:(State => Any) = (s:State) => {}
    var onServerQuestion:(State => State) = (s:State) => {State()}

    def delayedAction(action: => Any, timeout:Long, wait_start:Long = System.currentTimeMillis()) {
      actor {
        if(System.currentTimeMillis() - wait_start > timeout) action
        else delayedAction(action, timeout, wait_start)
      }
    }

    def performSend(data:State) {
      log.debug("sending data to server:\n"+data)
      if(is_connected) {
        out.println(data.toJsonString)
        out.flush()
        val write_error = out.checkError()
        if(write_error) {
          log.warn("failed to send data to server: write error!")
          delayedAction(io_actor ! ("connect", server_url, port, ping_timeout, onServerDataReceived), 10)
        }
      } else log.warn("not connected to send data!")  // maybe perform connect here?
    }

    loop {
      react {
        case ("connect", new_server_url:String, new_port:Int, new_ping_timeout:Int, new_onServerDataReceived:(State => Any), new_onServerQuestion:(State => State)) =>
          if(is_connected) {
            is_connected = false
            if(socket != null) {
              val socket_url = socket.getInetAddress.getHostAddress
              socket.close()
              log.info("disconnected from server "+socket_url)
            }
          }
          server_url = new_server_url
          port = new_port
          ping_timeout = new_ping_timeout
          onServerDataReceived = new_onServerDataReceived
          onServerQuestion = new_onServerQuestion
          log.info("start connecting to server "+server_url+" at port "+port)
          try {
            socket = new Socket(server_url, port)
            out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream, "UTF-8"))
            in = new BufferedReader(new InputStreamReader(socket.getInputStream, "UTF-8"))
            is_connected = true
            log.info("connected!")
            delayedAction(io_actor ! "check", 10)
            if(ping_timeout > 0) delayedAction(io_actor ! "ping", ping_timeout)
          } catch {
            case e:Exception => {
              log.error("failed to connect to server "+server_url+" at port "+port+": "+e)
              delayedAction(io_actor ! ("connect", server_url, port, ping_timeout, onServerDataReceived), 1000)
            }
          }
        case ("send", data:State) => performSend(data)
        case ("sendSync", data:State) =>
          performSend(data)
          reply("finished sending")
        case "check" =>
          if(is_connected) {
            if(in.ready) {
              try {
                val message = in.readLine
                log.debug("incoming message from server:\n"+message)
                val received_data = State.fromJsonStringOrDefault(message, State(("raw" -> message)))
                if(received_data.contains("ping")) log.debug("received ping from server")
                else if(received_data.contains("question")) {
                  log.debug("received question from server:\n"+received_data)
                  actor {
                    val question = received_data.value[State]("question")
                    val answer = State("answer" -> onServerQuestion(question))
                    log.debug("sending the answer:\n"+answer)
                    send(answer)
                  }
                } else if(received_data.contains("answer")) {
                  log.debug("received answer from server:\n"+received_data)
                  actor {
                    val answer_state = received_data.value[State]("answer")
                    server_answer.put(answer_state)
                  }
                } else {
                  log.debug("received data from server:\n"+received_data)
                  actor {
                    onServerDataReceived(received_data)
                  }
                }
              } catch {
                case e:Exception => {
                  log.error("error while receiving data from server: "+e)
                  // disconnect maybe?
                }
              }
            }
            delayedAction(io_actor ! "check", 10)
          } // will stop checking otherwise
        case "ping" =>
          if(is_connected) {
            performSend(State("ping"))
            if(ping_timeout > 0) delayedAction(io_actor ! "ping", ping_timeout)
          } // will stop pinging otherwise
        case ("disconnect") =>    // TODO: maybe send to server some message on disconnect like server themselves do to quickly shutdown and remove this client
          is_connected = false
          if(socket != null) {
            val socket_url = socket.getInetAddress.getHostAddress
            log.info("disconnected from server "+socket_url)
            socket.close()
          }
          reply("disconnected")
        case other => log.warn("Netclient io_actor received unexpected message: "+other)
      }
    }
  }

  def send(data:State) {
    io_actor ! ("send", data)
  }
  def send(data:String) {send(State(("raw" -> data)))}

  def sendSync(data:State) {
    io_actor !? (("sendSync", data))
  }
  def sendSync(data:String) {sendSync(State(("raw" -> data)))}

  private val server_answer = new SyncVar[State]
  def askServer(question:State):State = {
    send(State("question" -> question))
    server_answer.take()
  }

  def startClient(
    server_url:String =  property("net.server", "localhost"),
    port:Int = property("net.port", 9800),
    ping_timeout:Int = property("net.ping_timeout", 60000),
    onServerDataReceived:(State) => Any = (state) => {},
    onServerQuestion:(State) => State = (state) => {State()}
  ) {
    io_actor ! ("connect", server_url, port, ping_timeout, onServerDataReceived, onServerQuestion)
  }

  def stopClient() {
    io_actor !? "disconnect"
  }
}