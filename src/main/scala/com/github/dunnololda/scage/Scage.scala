package com.github.dunnololda.scage

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

trait OperationMapping {
  private val log = MySimpleLogger(this.getClass.getName)

  protected var current_operation_id = 0

  def currentOperation = current_operation_id

  //private[scage] val del_operations = ArrayBuffer[() => Unit]()
  case class DelOperation(op_id:Int, show_warnings:Boolean = true)
  private[scage] val del_operations = ArrayBuffer[DelOperation]()
  def executeDelOperationsIfExists(): Unit = {
    if(del_operations.nonEmpty) {
      //del_operations.foreach(delOp => delOp())
      del_operations.foreach(delOp => _delOperation(delOp.op_id, delOp.show_warnings))
      del_operations.clear()
    }
  }

  trait OperationContainer[A <: ScageOperation] {
    def name: String

    protected def addOperation(operation: A)

    private[OperationMapping] def removeOperation(op_id: Int): Option[A]

    def operations: Seq[A]

    def length: Int

    protected val operation_mapping = mapping

    protected def addOperationWithMapping(operation: A) = {
      addOperation(operation)
      mapping += (operation.op_id -> this)
      operation.op_id
    }

    protected def _delOperation(op_id: Int, show_warnings: Boolean) = {
      removeOperation(op_id) match {
        case some_operation@Some(operation) =>
          log.debug("deleted operation with id " + op_id + " from the container " + name)
          mapping -= op_id
          some_operation
        case None =>
          if (show_warnings) log.warn("operation with id " + op_id + " not found in the container " + name)
          None
      }
    }

    def delOperation(op_id: Int) = {
      del_operations += DelOperation(op_id)
    }

    private[scage] def delOperationImmediately(op_id: Int) = {
      _delOperation(op_id, show_warnings = true)
    }

    def delOperationNoWarn(op_id: Int) = {
      del_operations += DelOperation(op_id, show_warnings = false)
    }

    def delOperations(op_ids: Int*) {
      del_operations ++= op_ids.map(op_id => DelOperation(op_id, show_warnings = true))
    }

    private[scage] def delOperationsImmediately(op_ids: Int*) {
      op_ids.foreach(_delOperation(_, show_warnings = true))
    }

    def delOperationsNoWarn(op_ids: Int*) {
      del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = false)))
    }

    def delOperations(op_ids: Traversable[Int]) {
      del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = true)))
    }

    def delOperationsNoWarn(op_ids: Traversable[Int]) {
      del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = false)))
    }

    def delAllOperations() {
      del_operations += (() => {
        operations.map(_.op_id).foreach(_delOperation(_, show_warnings = true))
        log.info("deleted all operations from the container " + name)
      })
    }

    private[scage] def delAllOperationsImmediately() {
      operations.map(_.op_id).foreach(_delOperation(_, show_warnings = true))
      log.info("deleted all operations from the container " + name)
    }

    def delAllOperationsExcept(except_op_ids: Int*) {
      del_operations += (() => {
        operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))
      })
    }

    private[scage] def delAllOperationsExceptImmediately(except_op_ids: Int*) {
      operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))
    }

    def delAllOperationsExceptNoWarn(except_op_ids: Int*) {
      del_operations += (() => {
        operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = false))
      })
    }
  }

  class DefaultOperationContainer(val name: String) extends OperationContainer[ScageOperation] {
    protected val _operations = SortedBuffer[ScageOperation]()

    protected def addOperation(operation: ScageOperation) {
      _operations += operation
    }

    protected def removeOperation(op_id: Int): Option[ScageOperation] = _operations.indexWhere(_.op_id == op_id) match {
      case index if index != -1 => Some(_operations.remove(index))
      case _ => None
    }

    def operations: Seq[ScageOperation] = _operations

    def length: Int = _operations.length

    private def addOp(op_id: Int, op: () => Any, position:Int): Int = {
      addOperationWithMapping(ScageOperation(op_id, op, position))
    }

    def addOp(op: () => Any, position:Int): Int = {
      addOp(nextId, op, position)
    }
  }

  protected def defaultContainer(container_name: String) = new DefaultOperationContainer(container_name)

  private[scage] val mapping = mutable.HashMap[Int, OperationContainer[_ <: ScageOperation]]() // maybe make this protected

  private def _delOperation(op_id: Int, show_warnings: Boolean) = {
    mapping.remove(op_id) match {
      case Some(container) =>
        container.removeOperation(op_id) match {
          case some_op@Some(_) =>
            log.debug("deleted operation with id " + op_id + " from the container " + container.name)
            some_op
          case None =>
            if (show_warnings) log.warn("operation with id " + op_id + " not found in the container " + container.name)
            None
        }
      case None =>
        if (show_warnings) log.warn("operation with id " + op_id + " not found among all containers")
        None
    }
  }

  def delOperation(op_id: Int) = {
    del_operations += (() => _delOperation(op_id, show_warnings = true))
  }

  def delOperationNoWarn(op_id: Int) = {
    del_operations += (() => _delOperation(op_id, show_warnings = false))
  }

  def deleteSelf() {
    delOperation(current_operation_id)
  }

  def delOperations(op_ids: Int*) {
    del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = true)))
  }

  def delOperationsNoWarn(op_ids: Int*) {
    del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = false)))
  }

  def delOperations(op_ids: Traversable[Int]) {
    del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = true)))
  }

  def delOperationsNoWarn(op_ids: Traversable[Int]) {
    del_operations += (() => op_ids.foreach(_delOperation(_, show_warnings = false)))
  }

  def delAllOperations() {
    del_operations += (() => {
      mapping.keys.foreach(_delOperation(_, show_warnings = true))
      log.info("deleted all operations")
    })
  }

  def delAllOperationsExcept(except_op_ids: Int*) {
    del_operations += (() => {
      mapping.keys.filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))
    })
  }

  def delAllOperationsExceptNoWarn(except_op_ids: Int*) {
    del_operations += (() => {
      mapping.keys.filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = false))
    })
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
  protected var is_running = false

  def isRunning = is_running

  // don't know exactly if I need this preinits, but I keep them for symmetry (because I already have disposes and I do need them - to stop NetServer/NetClient for example)
  private[scage] val preinits = defaultContainer("preinits")

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
    scage_log.info(unit_name + ": preinit")
    for (ScageOperation(preinit_id, preinit_operation, _) <- preinits.operations) {
      current_operation_id = preinit_id
      preinit_operation()
    }
    executeDelOperationsIfExists()
    preinit_moment = System.currentTimeMillis()
    pause_period_since_preinit = 0l
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

  private[scage] val inits = defaultContainer("inits")

  def init(init_func: => Any) = {
    if (is_running) init_func
    inits.addOp(() => init_func, 0)
  }
  def init(position:Int)(init_func: => Any) = {
    if (is_running) init_func
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
    scage_log.info(unit_name + ": init")
    for (ScageOperation(init_id, init_operation, _) <- inits.operations) {
      current_operation_id = init_id
      init_operation()
    }
    executeDelOperationsIfExists()
    init_moment = System.currentTimeMillis()
    pause_period_since_init = 0l
    scage_log.info("inits: " + inits.length + "; actions: " + actions.length + "; clears: " + clears.length)
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

  private[scage] val actions = defaultContainer("actions")

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

  private def _execute(_actions: Seq[ScageOperation]) {
    val ScageOperation(action_id, action_operation, _) = _actions.head
    current_operation_id = action_id
    action_operation()
    if (_actions.nonEmpty && _actions.tail.nonEmpty && !restart_toggled) _execute(_actions.tail)
  }
  private[scage] def executeActions() {
    // assuming to run in cycle, so we leave off any log messages
    restart_toggled = false
    if (actions.operations.nonEmpty) {
      _execute(actions.operations)
    }
    executeDelOperationsIfExists()
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

  private[scage] val clears = defaultContainer("clears")

  def clear(clear_func: => Any) = {
    clears.addOp(() => clear_func, 0)
  }
  def clear(position:Int)(clear_func: => Any) = {
    clears.addOp(() => clear_func, position)
  }

  private[scage] def executeClears() {
    scage_log.info(unit_name + ": clear")
    for (ScageOperation(clear_id, clear_operation, _) <- clears.operations) {
      current_operation_id = clear_id
      clear_operation()
    }
    executeDelOperationsIfExists()
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

  private[scage] val disposes = defaultContainer("disposes")

  def dispose(dispose_func: => Any) = {
    disposes.addOp(() => dispose_func, 0)
  }

  def dispose(position:Int)(dispose_func: => Any) = {
    disposes.addOp(() => dispose_func, position)
  }

  // 'disposes' suppose to run after unit is completely finished. No public method exists to run them inside run-loop
  private[scage] def executeDisposes() {
    scage_log.info(unit_name + ": dispose")
    for (ScageOperation(dispose_id, dispose_operation, _) <- disposes.operations) {
      current_operation_id = dispose_id
      dispose_operation()
    }
    executeDelOperationsIfExists()
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
