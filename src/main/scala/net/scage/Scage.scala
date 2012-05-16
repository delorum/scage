package net.scage

import com.weiglewilczek.slf4s.Logger
import collection.mutable.{HashMap, ArrayBuffer}
import support.ScageId._

class ScageApp(val unit_name:String = "Scage App") extends Scage /*with ScageMain */with App {
  override def main(args:Array[String]) {
    scage_log.info("starting main unit "+unit_name+"...")
    super.main(args)
    run()
    scage_log.info(unit_name+" was stopped")
    System.exit(0)
  }
}

/*trait ScageMain extends Scage {
  override def stop() {
    super.stop()
    Scage.stopApp()
  }
}*/

class ScageUnit(val unit_name:String = "Scage Unit") extends Scage {
  override def run() {
    scage_log.info("starting unit "+unit_name+"...")
    super.run()
    scage_log.info(unit_name+" was stopped")
  }
}

trait Scage {
  def unit_name:String

  protected val scage_log = Logger(this.getClass.getName)

  private[scage] var current_operation_id = 0
  def currentOperation = current_operation_id

  object ScageOperations extends Enumeration {
    val Preinit, Init, Action, Clear, Dispose = Value
  }

  private[scage] val operations_mapping = HashMap[Int, Any]()
  def delOperation(operation_id:Int) = {
    operations_mapping.get(operation_id) match {
      case Some(operation_type) => {
        operation_type match {
          case ScageOperations.Init => delInits(operation_id)
          case ScageOperations.Action => delActions(operation_id)
          case ScageOperations.Clear => delClears(operation_id)
          case ScageOperations.Dispose => delDisposes(operation_id)
          case _ => {
            scage_log.warn("operation with id "+operation_id+" not found so wasn't deleted")
            false
          }
        }
      }
      case None => {
        scage_log.warn("operation with id "+operation_id+" not found so wasn't deleted")
        false
      }
    }
  }
  def deleteSelf() = delOperation(current_operation_id)
  def delOperations(operation_ids:Int*) = {
    operation_ids.foldLeft(true)((overall_result, operation_id) => {
      val deletion_result = delOperation(operation_id)
      overall_result && deletion_result
    })
  }

  // I see exactly zero real purposes to have such method
  def delAllOperations() {
    delAllPreinits()
    delAllInits()
    delAllActions()
    delAllClears()
    delAllDisposes()
  }
  def delAllOperationsExcept(operation_ids:Int*) {
    delAllPreinitsExcept(operation_ids:_*)
    delAllInitsExcept(operation_ids:_*)
    delAllActionsExcept(operation_ids:_*)
    delAllClearsExcept(operation_ids:_*)
    delAllDisposesExcept(operation_ids:_*)
  }

  // don't know exactly if I need this preinits, but I keep them for symmetry (because I already have disposes and I do need them - to stop NetServer/NetClient for example)
  private[scage] var preinits = ArrayBuffer[(Int, () => Any)]()
  def preinit(preinit_func: => Any) = {
    val operation_id = nextId
    preinits += (operation_id, () => preinit_func)
    operations_mapping += operation_id -> ScageOperations.Preinit
    if(is_running) preinit_func
    operation_id
  }
  // 'preinits' suppose to run only once during unit's first run(). No public method exists to run them inside run-loop
  private[scage] def preinit() {
    scage_log.info(unit_name+": preinit")
    for((preinit_id, preinit_operation) <- preinits) {
      current_operation_id = preinit_id
      preinit_operation()
    }
  }
  def delPreinits(operation_ids:Int*) = {
    operation_ids.foldLeft(true)((overall_result, operation_id) => {
      val deletion_result = preinits.find(_._1 == operation_id) match {
        case Some(p) => {
          preinits -= p
          scage_log.debug("deleted preinit operation with id "+operation_id)
          true
        }
        case None => {
          scage_log.warn("operation with id "+operation_id+" not found among preinits so wasn't deleted")
          false
        }
      }
      overall_result && deletion_result
    })
  }
  def delAllPreinits() {
    preinits.clear()
    scage_log.info("deleted all preinit operations")
  }
  def delAllPreinitsExcept(operation_ids:Int*) = {
    delPreinits(preinits.filter(preinit => !operation_ids.contains(preinit._1)).map(_._1):_*)
  }

  private[scage] val inits = ArrayBuffer[(Int, () => Any)]()
  def init(init_func: => Any) = {
    val operation_id = nextId
    inits += (operation_id, () => init_func)
    operations_mapping += operation_id -> ScageOperations.Init
    if(is_running) init_func
    operation_id
  }
  private[scage] def init() {
    scage_log.info(unit_name+": init")
    for((init_id, init_operation) <- inits) {
      current_operation_id = init_id
      init_operation()
    }
  }
  def delInits(operation_ids:Int*) = {
    operation_ids.foldLeft(true)((overall_result, operation_id) => {
      val deletion_result = inits.find(_._1 == operation_id) match {
        case Some(i) => {
          inits -= i
          scage_log.debug("deleted init operation with id "+operation_id)
          true
        }
        case None => {
          scage_log.warn("operation with id "+operation_id+" not found among inits so wasn't deleted")
          false
        }
      }
      overall_result && deletion_result
    })
  }
  def delAllInits() {
    inits.clear()
    scage_log.info("deleted all init operations")
  }
  def delAllInitsExcept(operation_ids:Int*) = {
    delInits(inits.filter(init => !operation_ids.contains(init._1)).map(_._1):_*)
  }

  private[scage] var actions = ArrayBuffer[(Int, () => Any)]()
  private def addAction(operation: => Any, is_pausable:Boolean) = {
    val operation_id = nextId
    if(is_pausable) actions += (operation_id, () => {if(!on_pause) operation})
    else actions += (operation_id, () => operation)
    operations_mapping += operation_id -> ScageOperations.Action
    operation_id
  }

  // pausable actions
  def action(action_func: => Any) = {
    addAction(action_func, true)
  }
  def action(action_period:Long)(action_func: => Unit) = {
    if(action_period > 0) {
      val action_waiter = new ActionWaiterStatic(action_period, action_func)
      addAction(action_waiter.doAction(), true)
    } else addAction(action_func, true)
  }
  def actionDynamicPeriod(action_period: => Long)(action_func: => Unit) = {
    val action_waiter = new ActionWaiterDynamic(action_period, action_func)
    addAction(action_waiter.doAction(), true)
  }

  // non-pausable variants
  def actionNoPause(action_func: => Any) = {
    addAction(action_func, false)
  }
  def actionNoPause(action_period:Long)(action_func: => Unit) = {
    if(action_period > 0) {
      val action_waiter = new ActionWaiterStatic(action_period, action_func)
      addAction(action_waiter.doAction(), false)
    } else addAction(action_func, false)
  }
  def actionDynamicPeriodNoPause(action_period: => Long)(action_func: => Unit) = {
    val action_waiter = new ActionWaiterDynamic(action_period, action_func)
    addAction(action_waiter.doAction(), false)
  }

  private[this] sealed abstract class ActionWaiter(action_func: => Unit) {
    private var last_action_time:Long = 0
    protected def period:Long

    def doAction() {
      if(System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }
  private[this] class ActionWaiterDynamic(action_period: => Long, action_func: => Unit) extends ActionWaiter(action_func) {
    def period = action_period
  }
  private[this] class ActionWaiterStatic(val period:Long, action_func: => Unit) extends ActionWaiter(action_func)

  private[scage] def executeActions() { // assuming to run in cycle, so we leave off any log messages
    for((action_id, action_operation) <- actions) {
      current_operation_id = action_id
      action_operation()
    }
  }

  def delActions(operation_ids:Int*) = {
    operation_ids.foldLeft(true)((overall_result, operation_id) => {
      val deletion_result = actions.find(_._1 == operation_id) match {
        case Some(a) => {
          actions -= a
          scage_log.debug("deleted action operation with id "+operation_id)
          true
        }
        case None => {
          scage_log.warn("operation with id "+operation_id+" not found among actions so wasn't deleted")
          false
        }
      }
      overall_result && deletion_result
    })
  }
  def delAllActions() {
    actions.clear()
    scage_log.info("deleted all action operations")
  }
  def delAllActionsExcept(operation_ids:Int*) {
    delActions(actions.filter(action => !operation_ids.contains(action._1)).map(_._1):_*)
  }

  private[scage] var clears = ArrayBuffer[(Int, () => Any)]()
  def clear(clear_func: => Any) = {
    val operation_id = nextId
    clears += (operation_id, () => clear_func)
    operations_mapping += operation_id -> ScageOperations.Clear
    operation_id
  }
  private[scage] def clear() {
    scage_log.info(unit_name+": clear")
    for((clear_id, clear_operation) <- clears) {
      current_operation_id = clear_id
      clear_operation()
    }
  }
  def delClears(operation_ids:Int*) = {
    operation_ids.foldLeft(true)((overall_result, operation_id) => {
      val deletion_result = clears.find(_._1 == operation_id) match {
        case Some(e) => {
          clears -= e
          scage_log.debug("deleted clear operation with id "+operation_id)
          true
        }
        case None => {
          scage_log.warn("operation with id "+operation_id+" not found among clears so wasn't deleted")
          false
        }
      }
      overall_result && deletion_result
    })
  }
  def delAllClears() {
    clears.clear()
    scage_log.info("deleted all clear operations")
  }
  def delAllClearsExcept(operation_ids:Int*) = {
    delClears(clears.filter(clear => !operation_ids.contains(clear._1)).map(_._1):_*)
  }

  private[scage] var disposes = ArrayBuffer[(Int, () => Any)]()
  def dispose(dispose_func: => Any) = {
    val operation_id = nextId
    disposes += (operation_id, () => dispose_func)
    operations_mapping += operation_id -> ScageOperations.Dispose
    operation_id
  }
  // 'disposes' suppose to run after unit is completely finished. No public method exists to run them inside run-loop
  private[scage] def dispose() {
    scage_log.info(unit_name+": dispose")
    for((dispose_id, dispose_operation) <- disposes) {
      current_operation_id = dispose_id
      dispose_operation()
    }
  }
  def delDisposes(operation_ids:Int*) = {
    operation_ids.foldLeft(true)((overall_result, operation_id) => {
      val deletion_result = disposes.find(_._1 == operation_id) match {
        case Some(d) => {
          disposes -= d
          scage_log.debug("deleted dispose operation with id "+operation_id)
          true
        }
        case None => {
          scage_log.warn("operation with id "+operation_id+" not found among disposes so wasn't deleted")
          false
        }
      }
      overall_result && deletion_result
    })
  }
  def delAllDisposes() {
    disposes.clear()
    scage_log.info("deleted all dispose operations")
  }
  def delAllDisposesExcept(operation_ids:Int*) = {
    delDisposes(disposes.filter(dispose => !operation_ids.contains(dispose._1)).map(_._1):_*)
  }

  protected var on_pause = false
  private def logPause() {scage_log.info("pause = " + on_pause)}
  def onPause = on_pause
  def switchPause() {on_pause = !on_pause; logPause()}
  def pause() {on_pause = true; logPause()}
  def pauseOff() {on_pause = false; logPause()}

  protected var is_running = false
  def isRunning = is_running
  def run() {
    preinit()
    init()
    is_running = true
    scage_log.info(unit_name+": run")
    while(is_running && Scage.isAppRunning) {
      executeActions()
    }
    clear()
    dispose()
  }

  def stop() {
    is_running = false
  }
  
  def restart() {
    clear()
    init()
  }

  /* this 'events'-functionality seems useless as the amount of usecases in real projects is zero
     but I plan to keep it, because I still have hope that someday I construct such usecase =)
  */
  private val events = HashMap[String, HashMap[Int, PartialFunction[Any, Unit]]]()
  def onEventWithArguments(event_name:String)(event_action: PartialFunction[Any, Unit]) = {
    val event_id = nextId
    events.get(event_name) match {
      case Some(events_for_name) =>
        events_for_name += (event_id -> event_action)
      case None => events += (event_name -> HashMap(event_id -> event_action))
    }
    (event_name, event_id)
  }
  def onEvent(event_name:String)(event_action: => Unit) = {
    val event_id = nextId
    events.get(event_name) match {
      case Some(events_for_name) => events_for_name += (event_id -> {case _ => event_action})
      case None => events += (event_name -> HashMap(event_id -> {case _ => event_action}))
    }
    (event_name, event_id)
  }
  def callEvent(event_name:String, arg:Any) {
    events.get(event_name) match {
      case Some(events_for_name) =>
        for(event <- events_for_name.values) event(arg) // fail-fast if not matched!
      case None => //scage_log.warn("event "+event_name+" not found")
    }
  }
  def callEvent(event_name:String) {
    events.get(event_name) match {
      case Some(events_for_name) =>
        for(event <- events_for_name.values) event() // fail-fast if not matched!
      case None => //scage_log.warn("event "+event_name+" not found")
    }
  }
  def delEvents(event_ids:(String, Int)*) {
    for((event_name, event_id) <- event_ids) {
      events.get(event_name) match {
        case Some(events_for_name) =>
          if(events_for_name.contains(event_id)) {
            events_for_name -= event_id
            scage_log.debug("deleted event for name "+event_name+" with id "+event_id)
          } else {
            scage_log.warn("event for name "+event_name+" with id "+event_id+" not found among events so wasn't deleted")
          }
        case None =>
          scage_log.warn("events for name "+event_name+" not found so event with id "+event_id+" wasn't deleted")
      }
    }
  }
}

object Scage {
  private var is_all_units_stop = false
  def isAppRunning = !is_all_units_stop
  def stopApp() {is_all_units_stop = true}
}
