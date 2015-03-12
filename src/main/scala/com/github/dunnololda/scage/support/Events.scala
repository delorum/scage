package com.github.dunnololda.scage.support

import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.support.ScageId._

import scala.collection.mutable

trait EventsTrait {
  def onEventWithArguments(event_name: String)(event_action: PartialFunction[Any, Unit]):(String, Int)
  def onEvent(event_name: String)(event_action: => Unit):(String, Int)
  def callEvent(event_name: String, arg: Any)
  def callEvent(event_name: String)
  def delEvents(event_ids: (String, Int)*)
}

object Events extends EventsTrait {
  private val log = MySimpleLogger(this.getClass.getName)
  private val events = mutable.HashMap[String, mutable.HashMap[Int, PartialFunction[Any, Unit]]]()

  def onEventWithArguments(event_name: String)(event_action: PartialFunction[Any, Unit]):(String, Int) = {
    val event_id = nextId
    (events.get(event_name) match {
      case Some(events_for_name) =>
        events_for_name += (event_id -> event_action)
      case None => events += (event_name -> mutable.HashMap(event_id -> event_action))
    }): Unit // this fixes some very huge compilation problem (very slow compilation)
    (event_name, event_id)
  }

  def onEvent(event_name: String)(event_action: => Unit):(String, Int) = {
    val event_id = nextId
    (events.get(event_name) match {
      case Some(events_for_name) => events_for_name += (event_id -> {
        case _ => event_action
      })
      case None => events += (event_name -> mutable.HashMap(event_id -> {
        case _ => event_action
      }))
    }): Unit
    (event_name, event_id)
  }

  def callEvent(event_name: String, arg: Any) {
    events.get(event_name) match {
      case Some(events_for_name) =>
        for (event <- events_for_name.values) event(arg) // fail-fast if not matched!
      case None => //log.warn("event "+event_name+" not found")
    }
  }

  def callEvent(event_name: String) {
    events.get(event_name) match {
      case Some(events_for_name) =>
        for (event <- events_for_name.values) event() // fail-fast if not matched!
      case None => //log.warn("event "+event_name+" not found")
    }
  }

  def delEvents(event_ids: (String, Int)*) {
    for ((event_name, event_id) <- event_ids) {
      events.get(event_name) match {
        case Some(events_for_name) =>
          if (events_for_name.contains(event_id)) {
            events_for_name -= event_id
            log.debug("deleted event for name " + event_name + " with id " + event_id)
          } else {
            log.warn("event for name " + event_name + " with id " + event_id + " not found among events so wasn't deleted")
          }
        case None =>
          log.warn("events for name " + event_name + " not found so event with id " + event_id + " wasn't deleted")
      }
    }
  }
}
