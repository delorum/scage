package net.scage.support.net

import _root_.net.scage.support.ScageProperties._
import java.io.{InputStreamReader, OutputStreamWriter, BufferedReader, PrintWriter}
import java.net.Socket
import net.scage.support.State
import com.weiglewilczek.slf4s.Logger
import actors.Actor._
import actors.Actor

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

    def performSend(data:State) {
      log.debug("sending data to server:\n"+data)
      if(is_connected) {
        out.println(data.toJsonString)
        out.flush()
        val write_error = out.checkError()
        if(write_error) {
          log.warn("failed to send data to server: write error!")
          actor{Thread.sleep(1000); io_actor ! ("connect", server_url, port, ping_timeout, onServerDataReceived)}
        }
      } else log.warn("not connected to send data!")
    }

    loop {
      react {
        case ("connect", new_server_url:String, new_port:Int, new_ping_timeout:Int, new_onServerDataReceived:(State => Any)) =>
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
          log.info("start connecting to server "+server_url+" at port "+port)
          try {
            socket = new Socket(server_url, port)
            out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream, "UTF-8"))
            in = new BufferedReader(new InputStreamReader(socket.getInputStream, "UTF-8"))
            is_connected = true
            log.info("connected!")
            actor{Thread.sleep(10); io_actor ! "check"}
            actor{Thread.sleep(ping_timeout); io_actor ! "ping"}
          } catch {
            case e:Exception => {
              log.error("failed to connect to server "+server_url+" at port "+port+": "+e);
              actor{Thread.sleep(1000); io_actor ! ("connect", server_url, port, ping_timeout, onServerDataReceived)}
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
                else {
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
            actor{Thread.sleep(10); io_actor ! "check"}
          } // will stop checking otherwise
        case "ping" =>
          if(is_connected) {
            performSend(State("ping"))
            actor{Thread.sleep(ping_timeout); io_actor ! "ping"}
          } // will stop pinging otherwise
        case ("disconnect") =>
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

  def startClient(
    server_url:String =  property("net.server", "localhost"),
    port:Int = property("net.port", 9800),
    ping_timeout:Int = property("net.ping_timeout", 60000, {ping_timeout:Int => (ping_timeout >= 1000, "must be more than 1000")}),
    onServerDataReceived:(State) => Any = (state) => {}
  ) {
    io_actor ! ("connect", server_url, port, ping_timeout, onServerDataReceived)
  }

  def stopClient() {
    io_actor !? "disconnect"
  }
}