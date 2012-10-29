package net.scage

import com.weiglewilczek.slf4s.Logger
import support.ScageId._
import collection.mutable.ArrayBuffer
import collection.mutable

case class ScageOperation(op_id:Int, op:() => Any)

trait HaveCurrentOperation {
  private var current_operation_id = 0
  def currentOperation = current_operation_id
  private[scage] def currentOperation_=(new_current_operation_id:Int) {current_operation_id = new_current_operation_id}
}

trait OperationMapping extends HaveCurrentOperation {
  private val log = Logger(this.getClass.getName)

  trait OperationContainer[A <: ScageOperation] {
    def name:String

    protected def addOperation(operation:A)
    protected def removeOperation(op_id:Int):Option[A]
    private[OperationMapping] def _removeOperation(op_id:Int):Option[A] = removeOperation(op_id)

    def operations:Seq[A]
    def length:Int

    protected val operation_mapping = mapping

    protected def addOperationWithMapping(operation:A) = {
      addOperation(operation)
      mapping += (operation.op_id -> this)
      operation.op_id
    }

    protected def _delOperation(op_id:Int, show_warnings:Boolean) = {
      removeOperation(op_id) match {
        case some_operation @ Some(operation) =>
          log.debug("deleted operation with id "+op_id+" from the container "+name)
          mapping -= op_id
          some_operation
        case None =>
          if(show_warnings) log.warn("operation with id "+op_id+" not found in the container "+name)
          None
      }
    }

    def delOperation(op_id:Int) = {_delOperation(op_id, show_warnings = true)}
    def delOperationNoWarn(op_id:Int) = {_delOperation(op_id, show_warnings = false)}

    def delOperations(op_ids:Int*) {op_ids.foreach(_delOperation(_, show_warnings = true))}
    def delOperationsNoWarn(op_ids:Int*) {op_ids.foreach(_delOperation(_, show_warnings = false))}
    def delOperations(op_ids:Traversable[Int]) {op_ids.foreach(_delOperation(_, show_warnings = true))}
    def delOperationsNoWarn(op_ids:Traversable[Int]) {op_ids.foreach(_delOperation(_, show_warnings = false))}

    def delAllOperations() {
      delOperations(operations.map(_.op_id))
      log.info("deleted all operations from the container "+name)
    }

    def delAllOperationsExcept(except_op_ids:Int*) {operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))}
    def delAllOperationsExceptNoWarn(except_op_ids:Int*) {operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = false))}
  }

  class DefaultOperationContainer(val name:String) extends OperationContainer[ScageOperation] {
    protected val _operations = ArrayBuffer[ScageOperation]()

    protected def addOperation(operation:ScageOperation) {_operations += operation}
    protected def removeOperation(op_id:Int):Option[ScageOperation] = _operations.indexWhere(_.op_id == op_id) match {
      case index if index != -1 => Some(_operations.remove(index))
      case _ => None
    }

    def operations:Seq[ScageOperation] = _operations
    def length:Int = _operations.length

    def addOp(op_id:Int, op:() => Any):Int = {
      addOperationWithMapping(ScageOperation(op_id, op))
    }

    def addOp(op:() => Any):Int = {
      addOp(nextId, op)
    }
  }

  protected def defaultContainer(container_name:String) = new DefaultOperationContainer(container_name)

  private[scage] val mapping = mutable.HashMap[Int, OperationContainer[_ <: ScageOperation]]()   // maybe make this protected

  private def _delOperation(op_id:Int, show_warnings:Boolean) = {
    mapping.remove(op_id) match {
      case Some(container) =>
        container._removeOperation(op_id) match {
          case some_op @ Some(_) =>
            log.debug("deleted operation with id "+op_id+" from the container "+container.name)
            some_op
          case None =>
            if(show_warnings) log.warn("operation with id "+op_id+" not found in the container "+container.name)
            None
        }
      case None =>
        if(show_warnings) log.warn("operation with id "+op_id+" not found among all containers")
        None
    }
  }

  def delOperation(op_id:Int) = {_delOperation(op_id, show_warnings = true)}
  def delOperationNoWarn(op_id:Int) = {_delOperation(op_id, show_warnings = false)}

  def deleteSelf() {delOperation(currentOperation)}

  def delOperations(op_ids:Int*) {op_ids.foreach(_delOperation(_, show_warnings = true))}
  def delOperationsNoWarn(op_ids:Int*)  {op_ids.foreach(_delOperation(_, show_warnings = false))}
  def delOperations(op_ids:Traversable[Int]) {op_ids.foreach(_delOperation(_, show_warnings = true))}
  def delOperationsNoWarn(op_ids:Traversable[Int]) {op_ids.foreach(_delOperation(_, show_warnings = false))}

  def delAllOperations() {
    delOperations(mapping.keys)
    log.info("deleted all operations")
  }

  def delAllOperationsExcept(except_op_ids:Int*) {mapping.keys.filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))}
  def delAllOperationsExceptNoWarn(except_op_ids:Int*) {mapping.keys.filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = false))}

  def operationExists(op_id:Int) = mapping.contains(op_id)
}

trait Pausable {
  this:Scage =>
  private var on_pause = false    // maybe make it private[scage]
  def onPause = on_pause
  def switchPause() {on_pause = !on_pause; scage_log.info("pause = " + on_pause)}
  def pause()       {on_pause = true;      scage_log.info("pause = " + on_pause)}
  def pauseOff()    {on_pause = false;     scage_log.info("pause = " + on_pause)}
}

trait Runnable {
  private var is_running = false
  def isRunning = is_running
  private[scage] def isRunning_=(new_is_running:Boolean) {is_running = new_is_running}

  private var restart_toggled = false
  private[scage] def restartToggled = restart_toggled
  private[scage] def restartToggled_=(new_restart_toggled:Boolean) {restart_toggled = new_restart_toggled}
}

trait Scage extends OperationMapping with Pausable with Runnable {
  def unit_name:String

  protected val scage_log = Logger(this.getClass.getName)

  // don't know exactly if I need this preinits, but I keep them for symmetry (because I already have disposes and I do need them - to stop NetServer/NetClient for example)
  private[scage] val preinits = defaultContainer("preinits")

  def preinit(preinit_func: => Any) = {
    if(isRunning) preinit_func
    preinits.addOp(() => preinit_func)
  }

  private var preinit_moment = System.currentTimeMillis()
  def preinitMoment = preinit_moment
  def msecsFromPreinit = System.currentTimeMillis() - preinit_moment

  // 'preinits' suppose to run only once during unit's first run(). No public method exists to run them inside run-loop
  private[scage] def executePreinits() {
    scage_log.info(unit_name+": preinit")
    for(ScageOperation(preinit_id, preinit_operation) <- preinits.operations) {
      currentOperation = preinit_id
      preinit_operation()
    }
    preinit_moment = System.currentTimeMillis()
  }

  def delPreinit(operation_id:Int) = {preinits.delOperation(operation_id)}
  def delPreinits(operation_ids:Int*) {preinits.delOperations(operation_ids:_*)}
  def delAllPreinits() {preinits.delAllOperations()}
  def delAllPreinitsExcept(except_operation_ids:Int*) {preinits.delAllOperationsExcept(except_operation_ids:_*)}

  private[scage] val inits = defaultContainer("inits")

  def init(init_func: => Any) = {
    if(isRunning) init_func
    inits.addOp(() => init_func)
  }

  private var init_moment = System.currentTimeMillis()
  def initMoment = init_moment
  def msecsFromInit = System.currentTimeMillis() - init_moment

  private[scage] def executeInits() {
    scage_log.info(unit_name+": init")
    for(ScageOperation(init_id, init_operation) <- inits.operations) {
      currentOperation = init_id
      init_operation()
    }
    init_moment = System.currentTimeMillis()
    scage_log.info("inits: "+inits.length+"; actions: "+actions.length+"; clears: "+clears.length)
  }

  def delInit(operation_id:Int) = {inits.delOperation(operation_id)}
  def delInits(operation_ids:Int*) {inits.delOperations(operation_ids:_*)}
  def delAllInits() {inits.delAllOperations()}
  def delAllInitsExcept(except_operation_ids:Int*) {inits.delAllOperationsExcept(except_operation_ids:_*)}

  private[scage] val actions = defaultContainer("actions")

  def actionIgnorePause(action_func: => Any):Int = {actions.addOp(() => action_func)}
  def actionIgnorePause(period:Long)(action_func: => Unit):Int = {
    if(period > 0) {
      var last_action_time:Long = 0
      actionIgnorePause {
        if(System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else actionIgnorePause {
        action_func
    }
  }
  def actionDynamicPeriodIgnorePause(period: => Long)(action_func: => Unit):Int = {
    var last_action_time:Long = 0
    actionIgnorePause {
      if(System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  // pausable actions
  def action(action_func: => Any):Int = {
    actionIgnorePause {
      if(!onPause) action_func
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

  // actions while on pause
  def actionOnPause(action_func: => Any):Int = {
    actionIgnorePause {
      if(onPause) action_func
    }
  }
  def actionOnPause(period:Long)(action_func: => Unit):Int = {
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
  def actionDynamicPeriodOnPause(period: => Long)(action_func: => Unit):Int = {
    var last_action_time:Long = 0
    action {
      if(System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  private[scage] def executeActions() { // assuming to run in cycle, so we leave off any log messages
    restartToggled = false
    def _execute(_actions:Traversable[ScageOperation]) {
      if(_actions.nonEmpty && !restartToggled) {
        val ScageOperation(action_id, action_operation) = _actions.head
        currentOperation = action_id
        action_operation()
        _execute(_actions.tail)
      }
    }
    _execute(actions.operations)
  }

  def delAction(operation_id:Int) = {actions.delOperation(operation_id)}
  def delActions(operation_ids:Int*) {actions.delOperations(operation_ids:_*)}
  def delAllActions() {actions.delAllOperations()}
  def delAllActionsExcept(except_operation_ids:Int*) {actions.delAllOperationsExcept(except_operation_ids:_*)}

  private[scage] val clears = defaultContainer("clears")

  def clear(clear_func: => Any) = {clears.addOp(() => clear_func)}

  private[scage] def executeClears() {
    scage_log.info(unit_name+": clear")
    for(ScageOperation(clear_id, clear_operation) <- clears.operations) {
      currentOperation = clear_id
      clear_operation()
    }
  }

  def delClear(operation_id:Int) = {clears.delOperation(operation_id)}
  def delClears(operation_ids:Int*) {clears.delOperations(operation_ids:_*)}
  def delAllClears() {clears.delAllOperations()}
  def delAllClearsExcept(except_operation_ids:Int*) {clears.delAllOperationsExcept(except_operation_ids:_*)}

  private[scage] val disposes = defaultContainer("disposes")

  def dispose(dispose_func: => Any) = {disposes.addOp(() => dispose_func)}

  // 'disposes' suppose to run after unit is completely finished. No public method exists to run them inside run-loop
  private[scage] def executeDisposes() {
    scage_log.info(unit_name+": dispose")
    for(ScageOperation(dispose_id, dispose_operation) <- disposes.operations) {
      currentOperation = dispose_id
      dispose_operation()
    }
  }

  def delDispose(operation_id:Int) = {disposes.delOperation(operation_id)}
  def delDisposes(operation_ids:Int*) {disposes.delOperations(operation_ids:_*)}
  def delAllDisposes() {disposes.delAllOperations()}
  def delAllDisposesExcept(except_operation_ids:Int*) {disposes.delAllOperationsExcept(except_operation_ids:_*)}

  def run() {
    executePreinits()
    executeInits()
    isRunning = true
    scage_log.info(unit_name+": run")
    while(isRunning && Scage.isAppRunning) {
      executeActions()
    }
    executeClears()
    executeDisposes()
  }

  def stop() {isRunning = false}

  def restart() {
    restartToggled = true
    executeClears()
    executeInits()
  }

  /* this 'events'-functionality seems useless as the amount of usecases in real projects is zero
     but I plan to keep it, because I still have hope that someday I construct such usecase =)
  */
  private val events = mutable.HashMap[String, mutable.HashMap[Int, PartialFunction[Any, Unit]]]()
  def onEventWithArguments(event_name:String)(event_action: PartialFunction[Any, Unit]) = {
    val event_id = nextId
    (events.get(event_name) match {
      case Some(events_for_name) =>
        events_for_name += (event_id -> event_action)
      case None => events += (event_name -> mutable.HashMap(event_id -> event_action))
    }):Unit
    (event_name, event_id)
  }
  def onEvent(event_name:String)(event_action: => Unit) = {
    val event_id = nextId
    (events.get(event_name) match {
      case Some(events_for_name) => events_for_name += (event_id -> {case _ => event_action})
      case None => events += (event_name -> mutable.HashMap(event_id -> {case _ => event_action}))
    }):Unit
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

trait SynchronizedScage extends Scage {
  override def isRunning = synchronized {super.isRunning}
  override private[scage] def isRunning_=(new_is_running:Boolean) {synchronized {super.isRunning = new_is_running}}

  override private[scage] def restartToggled = synchronized {super.restartToggled}
  override private[scage] def restartToggled_=(new_restart_toggled:Boolean) {synchronized {super.restartToggled = new_restart_toggled}}

  override def onPause = synchronized {super.onPause}
  override def switchPause() {synchronized {super.switchPause()}}
  override def pause()       {synchronized {super.pause()}}
  override def pauseOff()    {synchronized {super.pauseOff()}}

  override def currentOperation = synchronized {super.currentOperation}
  override private[scage] def currentOperation_=(new_current_operation_id:Int) {synchronized {super.currentOperation = new_current_operation_id}}

  override private[scage] val mapping = new mutable.HashMap[Int, OperationContainer[_ <: ScageOperation]] with mutable.SynchronizedMap[Int, OperationContainer[_ <: ScageOperation]]

  class SynchronizedOperationContainer(name:String) extends DefaultOperationContainer(name) {
    override protected val _operations = new ArrayBuffer[ScageOperation] with mutable.SynchronizedBuffer[ScageOperation]
  }
  override protected def defaultContainer(container_name:String) = new SynchronizedOperationContainer(container_name)
}

object Scage {
  private var is_all_units_stop = false
  def isAppRunning = synchronized {!is_all_units_stop}
  def stopApp() {synchronized {is_all_units_stop = true}}
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
