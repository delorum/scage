package com.github.dunnololda.scage

import com.github.dunnololda.scage.ScagePhase.ScagePhase
import com.github.dunnololda.scage.support.SortedBuffer
import support.ScageId._
import collection.mutable.ArrayBuffer
import collection.mutable
import com.github.dunnololda.cli.Imports._

// extracted case class to this definition because we want to extend it!
class ScageOperation(val op_id: Int, val op: () => Any, val position:Int) extends Ordered[ScageOperation] {
  override def equals(other:Any):Boolean = other match {
    case that:ScageOperation => (that canEqual this) && this.op_id == that.op_id
    case _ => false
  }
  override val hashCode:Int = op_id
  def canEqual(other: Any)  = other.isInstanceOf[ScageOperation]

  override def compare(that: ScageOperation): Int = position - that.position
}
object ScageOperation {
  def apply(op_id: Int, op: () => Any, pos:Int) = new ScageOperation(op_id, op, pos)
  def unapply(data:Any):Option[(Int, () => Any, Int)] = data match {
    case v:ScageOperation => Some(v.op_id, v.op, v.position)
    case _ => None
  }
}

object ScagePhase extends Enumeration {
  type ScagePhase = Value
  val NoPhase, Preinit, Init, Action, Clear, Dispose, Render, Interface, Controls = Value
}

trait OperationMapping {
  private val log = MySimpleLogger(this.getClass.getName)

  protected var current_operation_id = 0
  def currentOperation = current_operation_id

  protected var is_running = false
  def isRunning = is_running

  private[scage] var scage_phase:ScagePhase = ScagePhase.NoPhase
  def currentPhase = scage_phase

  //private[scage] val del_operations = ArrayBuffer[() => Unit]()
  case class DelOperation(op_id:Int, show_warnings:Boolean, execute_on_deletion:Boolean)
  private[scage] val del_operations = ArrayBuffer[DelOperation]()
  case class AddOperation(container:OperationContainer, operation:ScageOperation, execute_if_app_running:Boolean)
  private[scage] val add_operations = ArrayBuffer[AddOperation]()
  def executeDelAndAddOperationsIfExist(): Unit = {
    if(del_operations.nonEmpty) {
      del_operations.foreach(delOp => {
        mapping.remove(delOp.op_id) match {
          case Some(container) =>
            container.operations.remove(delOp.op_id) match {
              case Some(operation) =>
                log.debug("deleted operation with id " + delOp.op_id + " from the container " + container.name)
                if(delOp.execute_on_deletion) {
                  operation.op()
                }
                if(container.isEmpty) {
                  log.info(s"deleted all operations from the container ${container.name}")
                }
                if(mapping.isEmpty) {
                  log.info("deleted all operations from all containers")
                }
              case None =>
                if (delOp.show_warnings) log.warn("operation with id " + delOp.op_id + " not found in the container " + container.name)
            }
          case None =>
            if (delOp.show_warnings) log.warn("operation with id " + delOp.op_id + " not found among all containers")
        }
      })
      del_operations.clear()
    }
    if(add_operations.nonEmpty) {
      add_operations.foreach(addOp => {
        if(addOp.execute_if_app_running && isRunning) {
          addOp.operation.op()
        }
        addOp.container.operations += addOp.operation
        mapping += (addOp.operation.op_id -> addOp.container)
      })
      add_operations.clear()
    }
  }

  class OperationContainer(val name: String, val scage_phase:ScagePhase, execute_if_app_running:Boolean) {
    private[scage] val operations = SortedBuffer()
    
    def length: Int = operations.length
    
    def isEmpty = operations.isEmpty

    private[scage] def addOp(op: () => Any, position:Int): Int = {
      val op_id = nextId
      val operation = ScageOperation(op_id, op, position)
      if(currentPhase == scage_phase) {
        add_operations += AddOperation(this, operation, execute_if_app_running)
      } else {
        if(execute_if_app_running && isRunning) {
          op()
        }
        operations += operation
        mapping += (op_id -> this)
      }
      op_id
    }

    def delOperation(op_id: Int) = {
      delOp(op_id, show_warnings = true, execute_on_deletion = false)
    }

    def delOperationNoWarn(op_id: Int) = {
      delOp(op_id, show_warnings = false, execute_on_deletion = false)
    }

    def delOperations(op_ids: Int*) {
      op_ids.foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
    }

    def delOperationsNoWarn(op_ids: Int*) {
      op_ids.foreach(op_id => delOp(op_id, show_warnings = false, execute_on_deletion = false))
    }

    def delOperations(op_ids: Traversable[Int]) {
      op_ids.foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
    }

    def delOperationsNoWarn(op_ids: Traversable[Int]) {
      op_ids.foreach(op_id => delOp(op_id, show_warnings = false, execute_on_deletion = false))
    }

    def delAllOperations() {
      operations.operationIdsIterable.foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
    }

    def delAllOperationsExcept(except_op_ids: Int*) {
      operations.operationIdsIterable.filter(!except_op_ids.contains(_)).foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
    }

    def delAllOperationsExceptNoWarn(except_op_ids: Int*) {
      operations.operationIdsIterable.filter(!except_op_ids.contains(_)).foreach(op_id => delOp(op_id, show_warnings = false, execute_on_deletion = false))
    }
  }

  protected def defaultContainer(container_name: String, scage_phase:ScagePhase, execute_if_app_running:Boolean) = {
    new OperationContainer(container_name, scage_phase, execute_if_app_running)
  }

  private[scage] val mapping = mutable.HashMap[Int, OperationContainer]() // maybe make this protected

  private[scage] def delOp(op_id: Int, show_warnings: Boolean, execute_on_deletion:Boolean) {
    mapping.get(op_id) match {
      case Some(container) =>
        if(currentPhase == container.scage_phase) {
          del_operations += DelOperation(op_id, show_warnings, execute_on_deletion)
        } else {
          mapping.remove(op_id)
          container.operations.remove(op_id) match {
            case Some(operation) =>
              log.debug("deleted operation with id " + op_id + " from the container " + container.name)
              if(execute_on_deletion) {
                operation.op()
              }
              if(container.isEmpty) {
                log.info(s"deleted all operations from the container ${container.name}")
              }
              if(mapping.isEmpty) {
                log.info("deleted all operations from all containers")
              }
            case None =>
              if (show_warnings) log.warn("operation with id " + op_id + " not found in the container " + container.name)
          }
        }
      case None =>
        if (show_warnings) log.warn("operation with id " + op_id + " not found among all containers")
    }
  }

  def delOperation(op_id: Int) = {
    delOp(op_id, show_warnings = true, execute_on_deletion = false)
  }

  def delOperationNoWarn(op_id: Int) = {
    delOp(op_id, show_warnings = false, execute_on_deletion = false)
  }

  def deleteSelf() {
    delOperation(current_operation_id)
  }

  def deleteSelfNoWarn() {
    delOperationNoWarn(current_operation_id)
  }

  def delOperations(op_ids: Int*) {
    op_ids.foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
  }

  def delOperationsNoWarn(op_ids: Int*) {
    op_ids.foreach(op_id => delOp(op_id, show_warnings = false, execute_on_deletion = false))
  }

  def delOperations(op_ids: Traversable[Int]) {
    op_ids.foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
  }

  def delOperationsNoWarn(op_ids: Traversable[Int]) {
    op_ids.foreach(op_id => delOp(op_id, show_warnings = false, execute_on_deletion = false))
  }

  def delAllOperations() {
    mapping.keys.foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
  }

  def delAllOperationsExcept(except_op_ids: Int*) {
    mapping.keys.filter(!except_op_ids.contains(_)).foreach(op_id => delOp(op_id, show_warnings = true, execute_on_deletion = false))
  }

  def delAllOperationsExceptNoWarn(except_op_ids: Int*) {
    mapping.keys.filter(!except_op_ids.contains(_)).foreach(op_id => delOp(op_id, show_warnings = false, execute_on_deletion = false))
  }

  def operationExists(op_id: Int) = mapping.contains(op_id)
}

trait Scage extends OperationMapping {
  def unit_name: String

  protected val scage_log = MySimpleLogger(this.getClass.getName)

  protected var on_pause = false

  private[scage] var last_pause_start_moment = 0l
  def lastPauseStartMoment = last_pause_start_moment

  private[scage] var pause_period_since_preinit = 0l
  def pausePeriod = pause_period_since_preinit

  private[scage] var pause_period_since_init = 0l
  def pausePeriodSinceInit = pause_period_since_init

  def onPause = on_pause
  def switchPause() {
    on_pause = !on_pause
    if(on_pause) {
      last_pause_start_moment = System.currentTimeMillis()
    } else {
      pause_period_since_preinit += (System.currentTimeMillis() - last_pause_start_moment)
      pause_period_since_init    += (System.currentTimeMillis() - last_pause_start_moment)
    }
    scage_log.info("pause = " + on_pause)
  }
  def pause() {
    on_pause = true
    last_pause_start_moment = System.currentTimeMillis()
    scage_log.info("pause = " + on_pause)
  }
  def pauseOff() {
    on_pause = false
    pause_period_since_preinit += (System.currentTimeMillis() - last_pause_start_moment)
    pause_period_since_init    += (System.currentTimeMillis() - last_pause_start_moment)
    scage_log.info("pause = " + on_pause)
  }

  protected var restart_toggled = false

  // don't know exactly if I need this preinits, but I keep them for symmetry (because I already have disposes and I do need them - to stop NetServer/NetClient for example)
  private[scage] val preinits = defaultContainer("preinits", ScagePhase.Preinit, execute_if_app_running = true)

  def preinit(preinit_func: => Any) = {
    if (is_running) preinit_func
    preinits.addOp(() => preinit_func, 0)
  }
  def preinit(position:Int)(preinit_func: => Any) = {
    if (is_running) preinit_func
    preinits.addOp(() => preinit_func, position)
  }

  private var preinit_moment = System.currentTimeMillis()
  def preinitMoment = preinit_moment

  def msecsFromPreinit = System.currentTimeMillis() - preinit_moment
  def msecsFromPreinitWithoutPause = {
    if(on_pause) last_pause_start_moment - pause_period_since_preinit - preinit_moment
    else System.currentTimeMillis() - pause_period_since_preinit - preinit_moment
  }

  // 'preinits' suppose to run only once during unit's first run(). No public method exists to run them inside run-loop
  private[scage] def executePreinits() {
    scage_phase = ScagePhase.Preinit
    scage_log.info(unit_name + ": preinit")
    executeDelAndAddOperationsIfExist()
    for (ScageOperation(preinit_id, preinit_operation, _) <- preinits.operations) {
      current_operation_id = preinit_id
      preinit_operation()
    }
    executeDelAndAddOperationsIfExist()
    preinit_moment = System.currentTimeMillis()
    pause_period_since_preinit = 0l
    scage_phase = ScagePhase.NoPhase
  }

  def delPreinit(operation_id: Int) = {
    preinits.delOperation(operation_id)
  }

  def delPreinits(operation_ids: Int*) {
    preinits.delOperations(operation_ids: _*)
  }

  def delAllPreinits() {
    preinits.delAllOperations()
  }

  def delAllPreinitsExcept(except_operation_ids: Int*) {
    preinits.delAllOperationsExcept(except_operation_ids: _*)
  }

  private[scage] val inits = defaultContainer("inits", ScagePhase.Init, execute_if_app_running = true)

  def init(init_func: => Any) = {
    inits.addOp(() => init_func, 0)
  }
  def init(position:Int)(init_func: => Any) = {
    inits.addOp(() => init_func, position)
  }

  private var init_moment = System.currentTimeMillis()
  def initMoment = init_moment

  def msecsFromInit = System.currentTimeMillis() - init_moment
  def msecsFromInitWithoutPause = {
    if(on_pause) last_pause_start_moment - pause_period_since_init - init_moment
    else System.currentTimeMillis() - pause_period_since_init - init_moment
  }

  private[scage] def executeInits() {
    scage_phase = ScagePhase.Init
    scage_log.info(unit_name + ": init")
    for (ScageOperation(init_id, init_operation, _) <- inits.operations) {
      current_operation_id = init_id
      init_operation()
    }
    executeDelAndAddOperationsIfExist()
    init_moment = System.currentTimeMillis()
    pause_period_since_init = 0l
    scage_log.info("inits: " + inits.length + "; actions: " + actions.length + "; clears: " + clears.length)
    scage_phase = ScagePhase.NoPhase
  }

  def delInit(operation_id: Int) = {
    inits.delOperation(operation_id)
  }

  def delInits(operation_ids: Int*) {
    inits.delOperations(operation_ids: _*)
  }

  def delAllInits() {
    inits.delAllOperations()
  }

  def delAllInitsExcept(except_operation_ids: Int*) {
    inits.delAllOperationsExcept(except_operation_ids: _*)
  }

  private[scage] val actions = defaultContainer("actions", ScagePhase.Action, execute_if_app_running = false)

  def actionIgnorePause(action_func: => Any): Int = {
    actions.addOp(() => action_func, 0)
  }
  def actionIgnorePause(position:Int)(action_func: => Any): Int = {
    actions.addOp(() => action_func, position)
  }

  def actionStaticPeriodIgnorePause(period: Long, position:Int = 0)(action_func: => Unit): Int = {
    if (period > 0) {
      var last_action_time: Long = 0
      actionIgnorePause(position) {
        if (System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else actionIgnorePause {
      action_func
    }
  }

  def actionDynamicPeriodIgnorePause(period: => Long, position:Int = 0)(action_func: => Unit): Int = {
    var last_action_time: Long = 0
    actionIgnorePause(position) {
      if (System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  // pausable actions
  def action(action_func: => Any): Int = {
    actionIgnorePause {
      if (!on_pause) action_func
    }
  }

  def action(position:Int)(action_func: => Any): Int = {
    actionIgnorePause(position) {
      if (!on_pause) action_func
    }
  }

  def actionStaticPeriod(period: Long, position:Int = 0)(action_func: => Unit): Int = {
    if (period > 0) {
      var last_action_time: Long = 0
      action(position) {
        if (System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else action {
      action_func
    }
  }

  def actionDynamicPeriod(period: => Long, position:Int = 0)(action_func: => Unit): Int = {
    var last_action_time: Long = 0
    action(position) {
      if (System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  // actions while on pause
  def actionOnPause(action_func: => Any): Int = {
    actionIgnorePause {
      if (on_pause) action_func
    }
  }

  def actionOnPause(position:Int)(action_func: => Any): Int = {
    actionIgnorePause(position) {
      if (on_pause) action_func
    }
  }

  def actionStaticPeriodOnPause(period: Long, position:Int = 0)(action_func: => Unit): Int = {
    if (period > 0) {
      var last_action_time: Long = 0
      actionOnPause(position) {
        if (System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else actionOnPause {
      action_func
    }
  }

  def actionDynamicPeriodOnPause(period: => Long, position:Int = 0)(action_func: => Unit): Int = {
    var last_action_time: Long = 0
    actionOnPause(position) {
      if (System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  private def _execute(_actions_iterator: Iterator[ScageOperation]) {
    while(!restart_toggled && _actions_iterator.hasNext) {
      val ScageOperation(action_id, action_operation, _) = _actions_iterator.next()
      current_operation_id = action_id
      action_operation()
    }
  }
  private[scage] def executeActions() {
    scage_phase = ScagePhase.Action
    // assuming to run in cycle, so we leave off any log messages
    restart_toggled = false
    if (actions.operations.nonEmpty) {
      _execute(actions.operations.iterator)
    }
    executeDelAndAddOperationsIfExist()
    scage_phase = ScagePhase.NoPhase
  }

  def delAction(operation_id: Int) = {
    actions.delOperation(operation_id)
  }

  def delActions(operation_ids: Int*) {
    actions.delOperations(operation_ids: _*)
  }

  def delAllActions() {
    actions.delAllOperations()
  }

  def delAllActionsExcept(except_operation_ids: Int*) {
    actions.delAllOperationsExcept(except_operation_ids: _*)
  }

  private[scage] val clears = defaultContainer("clears", ScagePhase.Clear, execute_if_app_running = false)

  def clear(clear_func: => Any) = {
    clears.addOp(() => clear_func, 0)
  }
  def clear(position:Int)(clear_func: => Any) = {
    clears.addOp(() => clear_func, position)
  }

  private[scage] def executeClears() {
    scage_phase = ScagePhase.Clear
    scage_log.info(unit_name + ": clear")
    for (ScageOperation(clear_id, clear_operation, _) <- clears.operations) {
      current_operation_id = clear_id
      clear_operation()
    }
    executeDelAndAddOperationsIfExist()
    scage_phase = ScagePhase.NoPhase
  }

  def delClear(operation_id: Int) = {
    clears.delOperation(operation_id)
  }

  def delClears(operation_ids: Int*) {
    clears.delOperations(operation_ids: _*)
  }

  def delAllClears() {
    clears.delAllOperations()
  }

  def delAllClearsExcept(except_operation_ids: Int*) {
    clears.delAllOperationsExcept(except_operation_ids: _*)
  }

  private[scage] val disposes = defaultContainer("disposes", ScagePhase.Dispose, execute_if_app_running = false)

  def dispose(dispose_func: => Any) = {
    disposes.addOp(() => dispose_func, 0)
  }

  def dispose(position:Int)(dispose_func: => Any) = {
    disposes.addOp(() => dispose_func, position)
  }

  // 'disposes' suppose to run after unit is completely finished. No public method exists to run them inside run-loop
  private[scage] def executeDisposes() {
    scage_phase = ScagePhase.Dispose
    scage_log.info(unit_name + ": dispose")
    for (ScageOperation(dispose_id, dispose_operation, _) <- disposes.operations) {
      current_operation_id = dispose_id
      dispose_operation()
    }
    executeDelAndAddOperationsIfExist()
    scage_phase = ScagePhase.NoPhase
  }

  def delDispose(operation_id: Int) = {
    disposes.delOperation(operation_id)
  }

  def delDisposes(operation_ids: Int*) {
    disposes.delOperations(operation_ids: _*)
  }

  def delAllDisposes() {
    disposes.delAllOperations()
  }

  def delAllDisposesExcept(except_operation_ids: Int*) {
    disposes.delAllOperationsExcept(except_operation_ids: _*)
  }

  def run() {
    executePreinits()
    executeInits()
    is_running = true
    scage_log.info(unit_name + ": run")
    while (is_running && Scage.isAppRunning) {
      executeActions()
    }
    executeClears()
    executeDisposes()
  }

  def stop() {
    is_running = false
  }

  def restart() {
    restart_toggled = true
    executeClears()
    executeInits()
  }
}

object Scage {
  private var is_all_units_stop = false

  def isAppRunning = !is_all_units_stop

  def stopApp() {
    is_all_units_stop = true
  }
}

class ScageApp(val unit_name: String = property("app.name", "Scage App")) extends Scage with Cli {
  val app_start_moment = System.currentTimeMillis()
  def msecsFromAppStart = System.currentTimeMillis() - app_start_moment

  override def main(args: Array[String]) {
    scage_log.info("starting main unit " + unit_name + "...")
    super.main(args)
    run()
    scage_log.info(unit_name + " was stopped")
    System.exit(0)
  }
}

class ScageUnit(val unit_name: String = "Scage Unit") extends Scage {
  override def run() {
    scage_log.info("starting unit " + unit_name + "...")
    super.run()
    scage_log.info(unit_name + " was stopped")
  }
}
