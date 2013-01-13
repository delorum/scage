package net.scage

import com.weiglewilczek.slf4s.Logger
import support.ScageId._
import collection.mutable.ArrayBuffer
import collection.mutable
import support.ScageProperties._

trait CommandLineInterface {
  this: App =>
  private val log = Logger(this.getClass.getName)

  case class CliArg(short: String, long: String, description: String, has_value: Boolean)

  private var program_description = ""

  def programDescription = program_description

  def programDescription_=(descr: String) {
    program_description = descr
  }

  private val cli_args_short = mutable.HashMap[String, CliArg]()
  private val cli_args_long = mutable.HashMap[String, CliArg]()

  def commandLineArg(short: String, long: String, description: String, has_value: Boolean) {
    val new_cli_arg = CliArg(short, long, description, has_value)
    cli_args_short += (short -> new_cli_arg)
    cli_args_long += (long -> new_cli_arg)
  }

  def commandLineArgAndParse(short: String, long: String, description: String, has_value: Boolean) {
    commandLineArg(short, long, description, has_value)
    parseCommandLineArgs()
  }


  def commandLineArgs(args: (String, String, String, Boolean)*) {
    args.foreach {
      case (short: String, long: String, description: String, has_value: Boolean) =>
        commandLineArg(short, long, description, has_value)
    }
  }

  def commandLineArgsAndParse(args: (String, String, String, Boolean)*) {
    commandLineArgs(args: _*)
    parseCommandLineArgs()
  }

  private def printHelpAndExit() {
    if (program_description != "") println(program_description)
    println("Options:")
    cli_args_short.values.foreach {
      case CliArg(short, long, description, has_value) =>
        val short_only = "-" + short
        val except_descr = short_only + (List().padTo(10 - short_only.length, " ").mkString) + "--" + long + " " + (if (has_value) "arg" else "")
        println(except_descr + (List().padTo(20 - except_descr.length, " ").mkString) + description)
    }

    {
      val short = "h"
      val long = "help"
      val description = "show this usage information"
      val short_only = "-" + short
      val except_descr = short_only + (List().padTo(10 - short_only.length, " ").mkString) + "--" + long + " "
      println(except_descr + (List().padTo(20 - except_descr.length, " ").mkString) + description)
    }

    System.exit(0)
  }

  private def checkPropInMap(m: mutable.HashMap[String, CliArg], prop: String, pos: Int) {
    prop match {
      case "help" => printHelpAndExit()
      case p => m.get(p) match {
        case Some(CliArg(_, long, _, has_value)) =>
          if (!has_value) {
            addProperty(long, true)
          } else {
            if (pos >= this.args.length) log.warn("required value for command line property: " + prop)
            else {
              val value = this.args(pos + 1)
              addProperty(long, value)
            }
          }
        case None => log.warn("unknown command line property: " + prop)
      }
    }
  }

  def parseCommandLineArgs() {
    this.args.zipWithIndex.filter {
      case (arg, pos) => arg.startsWith("-")
    }.map {
      case (arg, pos) => (arg.toList, pos)
    }.foreach {
      case (arg, pos) =>
        arg match {
          case '-' :: '-' :: prop => checkPropInMap(cli_args_long, prop.mkString, pos)
          case '-' :: prop => checkPropInMap(cli_args_short, prop.mkString, pos)
          case prop => log.warn("unknown command line property: " + prop.mkString)
        }
    }
    cli_args_short.values.filter {
      case CliArg(short, long, _, has_value) =>
        !has_value && !this.args.contains("--" + long) && !this.args.contains("-" + short)
    }.foreach {
      case CliArg(_, long, _, _) => addProperty(long, false)
    }
  }
}

case class ScageOperation(op_id: Int, op: () => Any)

trait OperationMapping {
  private val log = Logger(this.getClass.getName)

  protected var current_operation_id = 0

  def currentOperation = current_operation_id

  trait OperationContainer[A <: ScageOperation] {
    def name: String

    protected def addOperation(operation: A)

    protected def removeOperation(op_id: Int): Option[A]

    private[OperationMapping] def _removeOperation(op_id: Int): Option[A] = removeOperation(op_id)

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
      _delOperation(op_id, show_warnings = true)
    }

    def delOperationNoWarn(op_id: Int) = {
      _delOperation(op_id, show_warnings = false)
    }

    def delOperations(op_ids: Int*) {
      op_ids.foreach(_delOperation(_, show_warnings = true))
    }

    def delOperationsNoWarn(op_ids: Int*) {
      op_ids.foreach(_delOperation(_, show_warnings = false))
    }

    def delOperations(op_ids: Traversable[Int]) {
      op_ids.foreach(_delOperation(_, show_warnings = true))
    }

    def delOperationsNoWarn(op_ids: Traversable[Int]) {
      op_ids.foreach(_delOperation(_, show_warnings = false))
    }

    def delAllOperations() {
      delOperations(operations.map(_.op_id))
      log.info("deleted all operations from the container " + name)
    }

    def delAllOperationsExcept(except_op_ids: Int*) {
      operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))
    }

    def delAllOperationsExceptNoWarn(except_op_ids: Int*) {
      operations.view.map(_.op_id).filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = false))
    }
  }

  class DefaultOperationContainer(val name: String) extends OperationContainer[ScageOperation] {
    protected val _operations = ArrayBuffer[ScageOperation]()

    protected def addOperation(operation: ScageOperation) {
      _operations += operation
    }

    protected def removeOperation(op_id: Int): Option[ScageOperation] = _operations.indexWhere(_.op_id == op_id) match {
      case index if index != -1 => Some(_operations.remove(index))
      case _ => None
    }

    def operations: Seq[ScageOperation] = _operations

    def length: Int = _operations.length

    def addOp(op_id: Int, op: () => Any): Int = {
      addOperationWithMapping(ScageOperation(op_id, op))
    }

    def addOp(op: () => Any): Int = {
      addOp(nextId, op)
    }
  }

  protected def defaultContainer(container_name: String) = new DefaultOperationContainer(container_name)

  private[scage] val mapping = mutable.HashMap[Int, OperationContainer[_ <: ScageOperation]]() // maybe make this protected

  private def _delOperation(op_id: Int, show_warnings: Boolean) = {
    mapping.remove(op_id) match {
      case Some(container) =>
        container._removeOperation(op_id) match {
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
    _delOperation(op_id, show_warnings = true)
  }

  def delOperationNoWarn(op_id: Int) = {
    _delOperation(op_id, show_warnings = false)
  }

  def deleteSelf() {
    delOperation(current_operation_id)
  }

  def delOperations(op_ids: Int*) {
    op_ids.foreach(_delOperation(_, show_warnings = true))
  }

  def delOperationsNoWarn(op_ids: Int*) {
    op_ids.foreach(_delOperation(_, show_warnings = false))
  }

  def delOperations(op_ids: Traversable[Int]) {
    op_ids.foreach(_delOperation(_, show_warnings = true))
  }

  def delOperationsNoWarn(op_ids: Traversable[Int]) {
    op_ids.foreach(_delOperation(_, show_warnings = false))
  }

  def delAllOperations() {
    delOperations(mapping.keys)
    log.info("deleted all operations")
  }

  def delAllOperationsExcept(except_op_ids: Int*) {
    mapping.keys.filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = true))
  }

  def delAllOperationsExceptNoWarn(except_op_ids: Int*) {
    mapping.keys.filter(!except_op_ids.contains(_)).foreach(_delOperation(_, show_warnings = false))
  }

  def operationExists(op_id: Int) = mapping.contains(op_id)
}

/**
this 'events'-functionality seems useless as the amount of usecases in real projects is zero
  but I plan to keep it, because I still have hope that someday I construct such usecase =)
  */
trait Events {
  private val log = Logger(this.getClass.getName)
  private val events = mutable.HashMap[String, mutable.HashMap[Int, PartialFunction[Any, Unit]]]()

  def onEventWithArguments(event_name: String)(event_action: PartialFunction[Any, Unit]) = {
    val event_id = nextId
    (events.get(event_name) match {
      case Some(events_for_name) =>
        events_for_name += (event_id -> event_action)
      case None => events += (event_name -> mutable.HashMap(event_id -> event_action))
    }): Unit // this fixes some very huge compilation problem (very slow compilation)
    (event_name, event_id)
  }

  def onEvent(event_name: String)(event_action: => Unit) = {
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

trait Scage extends OperationMapping with Events {
  def unit_name: String

  protected val scage_log = Logger(this.getClass.getName)

  protected var on_pause = false

  def onPause = on_pause

  def switchPause() {
    on_pause = !on_pause; scage_log.info("pause = " + on_pause)
  }

  def pause() {
    on_pause = true; scage_log.info("pause = " + on_pause)
  }

  def pauseOff() {
    on_pause = false; scage_log.info("pause = " + on_pause)
  }

  protected var restart_toggled = false
  protected var is_running = false

  def isRunning = is_running

  // don't know exactly if I need this preinits, but I keep them for symmetry (because I already have disposes and I do need them - to stop NetServer/NetClient for example)
  private[scage] val preinits = defaultContainer("preinits")

  def preinit(preinit_func: => Any) = {
    if (is_running) preinit_func
    preinits.addOp(() => preinit_func)
  }

  private var preinit_moment = System.currentTimeMillis()

  def preinitMoment = preinit_moment

  def msecsFromPreinit = System.currentTimeMillis() - preinit_moment

  // 'preinits' suppose to run only once during unit's first run(). No public method exists to run them inside run-loop
  private[scage] def executePreinits() {
    scage_log.info(unit_name + ": preinit")
    for (ScageOperation(preinit_id, preinit_operation) <- preinits.operations) {
      current_operation_id = preinit_id
      preinit_operation()
    }
    preinit_moment = System.currentTimeMillis()
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
    inits.addOp(() => init_func)
  }

  private var init_moment = System.currentTimeMillis()

  def initMoment = init_moment

  def msecsFromInit = System.currentTimeMillis() - init_moment

  private[scage] def executeInits() {
    scage_log.info(unit_name + ": init")
    for (ScageOperation(init_id, init_operation) <- inits.operations) {
      current_operation_id = init_id
      init_operation()
    }
    init_moment = System.currentTimeMillis()
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
    actions.addOp(() => action_func)
  }

  def actionIgnorePause(period: Long)(action_func: => Unit): Int = {
    if (period > 0) {
      var last_action_time: Long = 0
      actionIgnorePause {
        if (System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else actionIgnorePause {
      action_func
    }
  }

  def actionDynamicPeriodIgnorePause(period: => Long)(action_func: => Unit): Int = {
    var last_action_time: Long = 0
    actionIgnorePause {
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

  def action(period: Long)(action_func: => Unit): Int = {
    if (period > 0) {
      var last_action_time: Long = 0
      action {
        if (System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else action {
      action_func
    }
  }

  def actionDynamicPeriod(period: => Long)(action_func: => Unit): Int = {
    var last_action_time: Long = 0
    action {
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

  def actionOnPause(period: Long)(action_func: => Unit): Int = {
    if (period > 0) {
      var last_action_time: Long = 0
      action {
        if (System.currentTimeMillis - last_action_time > period) {
          action_func
          last_action_time = System.currentTimeMillis
        }
      }
    } else action {
      action_func
    }
  }

  def actionDynamicPeriodOnPause(period: => Long)(action_func: => Unit): Int = {
    var last_action_time: Long = 0
    action {
      if (System.currentTimeMillis - last_action_time > period) {
        action_func
        last_action_time = System.currentTimeMillis
      }
    }
  }

  private[scage] def executeActions() {
    // assuming to run in cycle, so we leave off any log messages
    restart_toggled = false
    def _execute(_actions: Traversable[ScageOperation]) {
      val ScageOperation(action_id, action_operation) = _actions.head
      current_operation_id = action_id
      action_operation()
      if (_actions.nonEmpty && _actions.tail.nonEmpty && !restart_toggled) _execute(_actions.tail)
    }
    if (actions.operations.nonEmpty) {
      _execute(actions.operations)
    }
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
    clears.addOp(() => clear_func)
  }

  private[scage] def executeClears() {
    scage_log.info(unit_name + ": clear")
    for (ScageOperation(clear_id, clear_operation) <- clears.operations) {
      current_operation_id = clear_id
      clear_operation()
    }
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
    disposes.addOp(() => dispose_func)
  }

  // 'disposes' suppose to run after unit is completely finished. No public method exists to run them inside run-loop
  private[scage] def executeDisposes() {
    scage_log.info(unit_name + ": dispose")
    for (ScageOperation(dispose_id, dispose_operation) <- disposes.operations) {
      current_operation_id = dispose_id
      dispose_operation()
    }
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

class ScageApp(
                val unit_name: String = property("app.name", "Scage App")
                ) extends Scage with CommandLineInterface with App {
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
