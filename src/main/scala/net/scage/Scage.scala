package net.scage

import com.weiglewilczek.slf4s.Logger
import collection.mutable.HashMap
import support.ScageId._

class OperationMapping[A] {
  protected val log = Logger(this.getClass.getName)

  case class Container(name:String, private[OperationMapping] val contents:HashMap[Int, A] = HashMap[Int, A]()) {
    def addOp(op:A) = {addOperation(name, op)}

    def delOp(op_id:Int) {if(contents.contains(op_id)) delOperation(op_id)}
    def delOps(op_ids:Int*) {delOperations(op_ids.filter(contents.contains(_)))}

    def delAllOps() {
      delOperations(contents.keys)
      log.info("deleted all operations from the container "+name)
    }

    def delAllOpsExcept(except_op_ids:Int*) {
      val op_ids = contents.keys.filter(!except_op_ids.contains(_))
      delOperations(op_ids)
    }

    def ops:collection.Map[Int, A] = contents

    def length = contents.size
  }

  private val containers = HashMap[String, Container]()
  private val mapping = HashMap[Int, Container]()

  def container(name:String) = containers.getOrElseUpdate(name, Container(name))

  def addOperation(container_name:String, op:A) = {
    val op_id = nextId
    val container = containers.getOrElseUpdate(container_name, Container(container_name))
    container.contents += (op_id -> op)
    mapping += (op_id -> container)
    op_id
  }

  def delOperation(op_id:Int) {
    mapping.remove(op_id) match {
      case Some(Container(name, contents)) =>
        contents.remove(op_id) match {
          case Some(op) =>
            log.debug("deleted operation with id "+op_id+" from the container "+name)
          case None =>
            log.warn("operation with id "+op_id+" not found in the container "+name)
        }
      case None =>
        log.warn("operation with id "+op_id+" not found among all containers")
    }
  }

  def delOperations(op_ids:Int*) {for(op_id <- op_ids) delOperation(op_id)}
  def delOperations(op_ids:Iterable[Int]) {for(op_id <- op_ids) delOperation(op_id)}

  def delAllOperations() {
    containers.clear()
    mapping.clear()
    log.info("deleted all operations")
  }

  def delAllOperationsExcept(except_op_ids:Int*) {
    val op_ids = mapping.keys.filter(!except_op_ids.contains(_))
    delOperations(op_ids)
  }

  def operationExists(op_id:Int) = mapping.contains(op_id)
}

trait Scage {
  def unit_name:String

  protected val scage_log = Logger(this.getClass.getName)

  private[scage] var current_operation_id = 0
  def currentOperation = current_operation_id

  private[scage] val operations_mapping = new OperationMapping[() => Any]()
  def delOperation(operation_id:Int) {
    operations_mapping.delOperation(operation_id)
  }

  def operationExists(operation_id:Int) = operations_mapping.operationExists(operation_id)

  def deleteSelf() {delOperation(current_operation_id)}
  def delOperations(operation_ids:Int*) {
    operations_mapping.delOperations(operation_ids:_*)
  }

  // I see exactly zero real purposes to have such method
  def delAllOperations() {
    operations_mapping.delAllOperations()
  }
  def delAllOperationsExcept(except_operation_ids:Int*) {
    operations_mapping.delAllOperationsExcept(except_operation_ids:_*)
  }

  // don't know exactly if I need this preinits, but I keep them for symmetry (because I already have disposes and I do need them - to stop NetServer/NetClient for example)
  private[scage] var preinits = operations_mapping.container("preinits")
  def preinit(preinit_func: => Any) = {
    if(is_running) preinit_func
    preinits.addOp(() => preinit_func)
  }
  // 'preinits' suppose to run only once during unit's first run(). No public method exists to run them inside run-loop
  private[scage] def executePreinits() {
    scage_log.info(unit_name+": preinit")
    for((preinit_id, preinit_operation) <- preinits.ops) {
      current_operation_id = preinit_id
      preinit_operation()
    }
  }

  def delPreinit(operation_id:Int) {preinits.delOp(operation_id)}
  def delPreinits(operation_ids:Int*) {preinits.delOps(operation_ids:_*)}
  def delAllPreinits() {preinits.delAllOps()}
  def delAllPreinitsExcept(except_operation_ids:Int*) {preinits.delAllOpsExcept(except_operation_ids:_*)}

  private[scage] val inits = operations_mapping.container("inits")

  def init(init_func: => Any) = {
    if(is_running) init_func
    inits.addOp(() => init_func)
  }

  private[scage] def executeInits() {
    scage_log.info(unit_name+": init")
    for((init_id, init_operation) <- inits.ops) {
      current_operation_id = init_id
      init_operation()
    }
    scage_log.info("inits: "+inits.length+"; actions: "+actions.length+"; clears: "+clears.length)
  }

  def delInit(operation_id:Int) {inits.delOp(operation_id)}
  def delInits(operation_ids:Int*) {inits.delOps(operation_ids:_*)}
  def delAllInits() {inits.delAllOps()}
  def delAllInitsExcept(except_operation_ids:Int*) {inits.delAllOpsExcept(except_operation_ids:_*)}

  private[scage] var actions = operations_mapping.container("actions")

  def actionNoPause(action_func: => Any):Int = {actions.addOp(() => action_func)}

  def actionNoPause(period:Long)(action_func: => Unit):Int = {
    if(period > 0) {
      var last_action_time:Long = 0
      actionNoPause {
        if(System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else actionNoPause {
        action_func
    }
  }

  def actionDynamicPeriodNoPause(period: => Long)(action_func: => Unit):Int = {
    var last_action_time:Long = 0
    actionNoPause {
      if(System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  // pausable actions
  def action(action_func: => Any):Int = {
    actionNoPause {
      if(!on_pause) action_func
    }
  }

  def action(period:Long)(action_func: => Unit):Int = {
    if(period > 0) {
      var last_action_time:Long = 0
      action {
        if(System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }


    } else action {
        action_func
    }
  }

  def actionDynamicPeriod(period: => Long)(action_func: => Unit):Int = {
    var last_action_time:Long = 0
    action {
      if(System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  private[scage] def executeActions() { // assuming to run in cycle, so we leave off any log messages
    restart_toggled = false
    def _execute(_actions:Traversable[(Int, () => Any)]) {
      if(_actions.nonEmpty && !restart_toggled) {
        val (action_id, action_operation) = _actions.head
        current_operation_id = action_id
        action_operation()
        _execute(_actions.tail)
      }
    }
    _execute(actions.ops)
  }

  def delAction(operation_id:Int) {actions.delOp(operation_id)}
  def delActions(operation_ids:Int*) {actions.delOps(operation_ids:_*)}
  def delAllActions() {actions.delAllOps()}
  def delAllActionsExcept(except_operation_ids:Int*) {actions.delAllOpsExcept(except_operation_ids:_*)}

  private[scage] var clears = operations_mapping.container("clears")

  def clear(clear_func: => Any) = {clears.addOp(() => clear_func)}

  private[scage] def executeClears() {
    scage_log.info(unit_name+": clear")
    for((clear_id, clear_operation) <- clears.ops) {
      current_operation_id = clear_id
      clear_operation()
    }
  }

  def delClear(operation_id:Int) {clears.delOp(operation_id)}
  def delClears(operation_ids:Int*) {clears.delOps(operation_ids:_*)}
  def delAllClears() {clears.delAllOps()}
  def delAllClearsExcept(except_operation_ids:Int*) {clears.delAllOpsExcept(except_operation_ids:_*)}

  private[scage] var disposes = operations_mapping.container("disposes")

  def dispose(dispose_func: => Any) = {disposes.addOp(() => dispose_func)}

  // 'disposes' suppose to run after unit is completely finished. No public method exists to run them inside run-loop
  private[scage] def executeDisposes() {
    scage_log.info(unit_name+": dispose")
    for((dispose_id, dispose_operation) <- disposes.ops) {
      current_operation_id = dispose_id
      dispose_operation()
    }
  }

  def delDispose(operation_id:Int) {disposes.delOp(operation_id)}
  def delDisposes(operation_ids:Int*) {disposes.delOps(operation_ids:_*)}
  def delAllDisposes() {disposes.delAllOps()}
  def delAllDisposesExcept(except_operation_ids:Int*) {disposes.delAllOpsExcept(except_operation_ids:_*)}

  protected var on_pause = false
  def onPause = on_pause
  def switchPause() {on_pause = !on_pause; scage_log.info("pause = " + on_pause)}
  def pause()       {on_pause = true;      scage_log.info("pause = " + on_pause)}
  def pauseOff()    {on_pause = false;     scage_log.info("pause = " + on_pause)}

  protected var is_running = false
  def isRunning = is_running
  def run() {
    executePreinits()
    executeInits()
    is_running = true
    scage_log.info(unit_name+": run")
    while(is_running && Scage.isAppRunning) {
      executeActions()
    }
    executeClears()
    executeDisposes()
  }

  def stop() {
    is_running = false
  }

  private[scage] var restart_toggled = false
  def restart() {
    restart_toggled = true
    executeClears()
    executeInits()
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
