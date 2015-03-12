package com.github.dunnololda.scage.support.tracer3

import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.cli.AppProperties._
import com.github.dunnololda.mysimplelogger.MySimpleLogger
import com.github.dunnololda.scage.handlers.RendererLib
import collection.mutable.ArrayBuffer
import collection.mutable
import scala.language.implicitConversions

class ScageTracer[T <: TraceTrait](val field_from_x:Int = property("field.from.x", 0),
                              val field_to_x:Int        = property("field.to.x", try {RendererLib.windowWidth} catch {case e:Exception => 800}),
                              val field_from_y:Int      = property("field.from.y", 0),
                              val field_to_y:Int        = property("field.to.y", try {RendererLib.windowHeight} catch {case e:Exception => 600}),
                              init_h_x:Int              = property("field.h_x", 0),
                              init_h_y:Int              = property("field.h_y", 0),
                              init_N_x:Int              = if(property("field.h_x", 0) == 0) property("field.N_x", (try {RendererLib.windowWidth} catch {case e:Exception => 800})/50) else 0,
                              init_N_y:Int              = if(property("field.h_y", 0) == 0) property("field.N_y", (try {RendererLib.windowHeight} catch {case e:Exception => 800})/50) else 0,
                              val solid_edges:Boolean   = property("field.solid_edges", true)) {
  private val log = MySimpleLogger(this.getClass.getName)

  val field_width = field_to_x - field_from_x
  val field_height = field_to_y - field_from_y

  val N_x = if(init_h_x != 0) field_width/init_h_x else init_N_x
  val N_y = if(init_h_y != 0) field_height/init_h_y else init_N_y

  val h_x = if(init_h_x == 0) field_width/init_N_x else init_h_x
  val h_y = if(init_h_y == 0) field_height/init_N_y else init_h_y

  log.info("creating tracer "+this.getClass.getName+": h_x="+h_x+" h_y="+h_y+" N_x="+N_x+" N_y="+N_y)

  // for client classes - children of ScageTracer
  protected def setTraceLocation(trace:T, new_location:Vec) {trace._location = new_location}
  protected implicit def trace2updateable(trace:T): Object {def __location_=(new_location: Vec): Unit; def __location: Vec} = new {
    def __location = trace._location
    def __location_=(new_location:Vec) {trace._location = new_location}
  }

  def isPointOnArea(x:Float, y:Float):Boolean = x >= 0 && x < N_x && y >= 0 && y < N_y
  def isPointOnArea(point:Vec):Boolean = isPointOnArea(point.x, point.y)
  def outsidePoint(point:Vec):Vec = {
    def checkC(c:Float, dist:Int):Float = {
      if(c >= dist) checkC(c - dist, dist)
      else if(c < 0) checkC(c + dist, dist)
      else c
    }
    if(solid_edges) point
    else Vec(checkC(point.x, N_x), checkC(point.y, N_y))
  }

  def point(x:Float, y:Float):Vec = Vec(((x - field_from_x)/field_width*N_x).toInt,
                                        ((y - field_from_y)/field_height*N_y).toInt)
  def point(p:Vec):Vec = point(p.x, p.y)
  def pointCenter(x:Float, y:Float):Vec = Vec(field_from_x + x*h_x + h_x/2, field_from_y + y*h_y + h_y/2)
  def pointCenter(p:Vec):Vec = pointCenter(p.x, p.y)

  protected def initMatrix(matrix:Array[Array[ArrayBuffer[T]]]) {
    for(i <- 0 until matrix.length; j <- 0 until matrix.head.length) {matrix(i)(j) = ArrayBuffer[T]()}
  }
  protected def clearMatrix(matrix:Array[Array[ArrayBuffer[T]]]) {
    for(i <- 0 until matrix.length; j <- 0 until matrix.head.length) {matrix(i)(j).clear()}
  }

  // it is very critical for the structures below to be changed only inside ScageTracer
  // but for convenience I keep them protected, so client classes - children of ScageTracer can read them
  protected val point_matrix = Array.ofDim[ArrayBuffer[T]](N_x, N_y)
  initMatrix(point_matrix)
  protected val traces_by_ids = mutable.HashMap[Int, T]()
  protected var traces_list = ArrayBuffer[T]()
  def tracesList = traces_list.toList
  
  def addTrace(point:Vec, trace:T) = {
    if(isPointOnArea(point)) {
      trace._location = point
      point_matrix(point.ix)(point.iy) += trace
      traces_by_ids += trace.id -> trace
      traces_list += trace
      log.debug("added new trace #"+trace.id+" in point ("+trace.location+")")
    } else log.warn("failed to add trace: point ("+point+") is out of area")
    trace
  }
  def addTraces(traces_in_locations:(Vec, T)*) = {
    for {
      (location, trace) <- traces_in_locations
    } yield addTrace(location, trace)
  }

  def containsTrace(trace_id:Int) = traces_by_ids.contains(trace_id)
  def containsTrace(trace:T) = traces_by_ids.contains(trace.id)

  protected def _removeTrace(trace_id:Int, show_warn:Boolean) {
    traces_by_ids.get(trace_id) match {
      case Some(trace) =>
        point_matrix(trace.location.ix)(trace.location.iy) -= trace
        traces_by_ids -= trace.id
        traces_list -= trace
        log.debug("removed trace #"+trace.id)
      case None => if(show_warn) log.warn("trace #"+trace_id+" not found")
    }
  }

  def removeTrace(trace:T)                {_removeTrace(trace.id, show_warn = true)}
  def removeTraceNoWarn(trace:T)          {_removeTrace(trace.id, show_warn = false)}
  def removeTraceById(trace_id:Int)       {_removeTrace(trace_id, show_warn = true)}
  def removeTraceByIdNoWarn(trace_id:Int) {_removeTrace(trace_id, show_warn = false)}

  // WARNING: its very important not to directly pass contents of point_matrix(trace.location.ix)(trace.location.iy),
  // traces_by_ids or traces_list as it cannot remove from itself properly, causing NullPointerException!
  // maybe return result (true/false)
  // maybe call toList before foreach to copy the list
  def removeTraces(traces_to_remove:T*)       {traces_to_remove.foreach(t => _removeTrace(t.id, show_warn = true))}
  def removeTracesNoWarn(traces_to_remove:T*) {traces_to_remove.foreach(t => _removeTrace(t.id, show_warn = false))}
  def removeTracesById(trace_ids:Int*)        {trace_ids.foreach(t_id     => _removeTrace(t_id, show_warn = true))}
  def removeTracesByIdNoWarn(trace_ids:Int*)  {trace_ids.foreach(t_id     => _removeTrace(t_id, show_warn = false))}
  def removeAllTraces() {
    clearMatrix(point_matrix)
    traces_by_ids.clear()
    traces_list.clear()
    log.info("deleted all traces")
  }
  def removeTracesInPoint(point:Vec) {removeTraces(tracesInPoint(point):_*)}
  def removeTracesInPoint(x:Int, y:Int) {removeTraces(tracesInPoint(x, y):_*)}

  def tracesInPoint(point:Vec, condition:T => Boolean):List[T] = tracesInPoint(point.ix, point.iy, condition)
  def tracesInPoint(x:Int, y:Int, condition:T => Boolean) = {
    if(!isPointOnArea(x, y)) Nil
    else {
      (for {
        trace <- point_matrix(x)(y)
        if condition(trace)
      } yield trace).toList
    }
  }

  def tracesInPoint(point:Vec):List[T] = tracesInPoint(point.ix, point.iy)
  def tracesInPoint(x:Int, y:Int) = {
    if(!isPointOnArea(x, y)) Nil
    else point_matrix(x)(y).toList
  }

  def tracesInPointRange(xrange:Range, yrange:Range, condition:T => Boolean):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, yrange, condition)
  def tracesInPointRange(xrange:Range, condition:T => Boolean):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, xrange, condition)
  def tracesInPointRange(xrange:Range, yrange:Range):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, yrange)
  def tracesInPointRange(xrange:Range):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, xrange)

  def tracesNearPoint(point:Vec, xrange:Range, yrange:Range, condition:T => Boolean):IndexedSeq[T] = {
    for {
      i <- xrange
      j <- yrange
      near_point = outsidePoint(point + Vec(i, j))
      if isPointOnArea(near_point)
      trace <- tracesInPoint(near_point, condition)
    } yield trace
  }
  def tracesNearPoint(point:Vec, xrange:Range, condition:T => Boolean):IndexedSeq[T] = tracesNearPoint(point, xrange, xrange, condition)
  def tracesNearPoint(point:Vec, xrange:Range, yrange:Range):IndexedSeq[T] = {
    for {
      i <- xrange
      j <- yrange
      near_point = outsidePoint(point + Vec(i, j))
      if isPointOnArea(near_point)
      trace <- tracesInPoint(near_point)
    } yield trace
  }
  def tracesNearPoint(point:Vec, xrange:Range):IndexedSeq[T] = tracesNearPoint(point, xrange, xrange)

  val LOCATION_UPDATED = 0
  val SAME_LOCATION    = 1
  val OUT_OF_AREA      = 2
  val TRACE_NOT_FOUND  = 3
  def updateLocation(trace_id:Int, new_point:Vec):Int = { // TODO: maybe return tuple (new_location, operation_status), maybe rename
    traces_by_ids.get(trace_id) match {
      case Some(updateable_trace) =>
        val old_point = updateable_trace.location
        val new_point_edges_affected = outsidePoint(new_point)
        if(isPointOnArea(new_point_edges_affected)) {
          if(old_point != new_point_edges_affected) {
            point_matrix(old_point.ix)(old_point.iy) -= updateable_trace
            point_matrix(new_point_edges_affected.ix)(new_point_edges_affected.iy) += updateable_trace
            updateable_trace._location = new_point_edges_affected
            LOCATION_UPDATED
          } else {
            //log.warn("didn't update trace "+trace.id+": new point is the same as the old one")  // don'tknow exactly if I need such debug message
            SAME_LOCATION
          }
        } else {
          log.debug("failed to update trace "+trace_id+": new point is out of area")
          OUT_OF_AREA
        }
      case None =>
        log.warn("trace with id "+trace_id+" not found")
        TRACE_NOT_FOUND
    }
  }
  def updateLocation(trace:T, new_point:Vec):Int = updateLocation(trace.id, new_point)  // TODO: maybe rename

  def moveTrace(trace:T, delta:Vec) = {
    updateLocation(trace.id, trace.location + delta)
  }

  def randomPoint(leftup_x:Int = 0, leftup_y:Int = N_y-1, width:Int = N_x, height:Int = N_y) = {
    val x = leftup_x + (math.random*width).toInt
    val y = leftup_y - (math.random*height).toInt
    Vec(x,y)
  }
  def randomPointWithCondition(leftup_x:Int = 0,
                  leftup_y:Int = N_y-1, 
                  width:Int = N_x, 
                  height:Int = N_y,
                  condition:Vec => Boolean,
                  num_tries:Int = 10):Option[Vec] = {
    (0 until num_tries).view.map(i => randomPoint()).find(p => condition(p))
  }
  def randomCoord(leftup_x:Int = field_from_x, leftup_y:Int = field_to_y-1, width:Int = field_to_x - field_from_x, height:Int = field_to_y - field_from_y) = {
    pointCenter(point(randomPoint(leftup_x, leftup_y, width, height)))
  }
  def randomCoordWithCondition(leftup_x:Int = field_from_x,
                  leftup_y:Int = field_to_y-1, 
                  width:Int = field_to_x - field_from_x, 
                  height:Int = field_to_y - field_from_y,
                  condition:Vec => Boolean,
                  num_tries:Int = 10):Option[Vec] = {
    (0 until num_tries).view.map(i => randomCoord()).find(coord => condition(coord))
  }

  lazy val trace_grid = {
    val x_lines = (field_from_x to field_to_x by h_x).foldLeft(List[Vec]())((lines, x) => Vec(x, field_from_y) :: Vec(x, field_to_y) :: lines)
    val y_lines = (field_from_y to field_to_y by h_y).foldLeft(List[Vec]())((lines, y) => Vec(field_from_x, y) :: Vec(field_to_x, y) :: lines)
    x_lines ::: y_lines
  }
}

object ScageTracer {
  def apply(field_from_x:Int        = property("field.from.x", 0),
            field_to_x:Int          = property("field.to.x", try {RendererLib.windowWidth} catch {case e:Exception => 800}),
            field_from_y:Int        = property("field.from.y", 0),
            field_to_y:Int          = property("field.to.y", try {RendererLib.windowHeight} catch {case e:Exception => 600}),
            init_h_x:Int            = property("field.h_x", 0),
            init_h_y:Int            = property("field.h_y", 0),
            init_N_x:Int            = if(property("field.h_x", 0) == 0) property("field.N_x", (try {RendererLib.windowWidth} catch {case e:Exception => 800})/50) else 0,
            init_N_y:Int            = if(property("field.h_y", 0) == 0) property("field.N_y", (try {RendererLib.windowHeight} catch {case e:Exception => 600})/50) else 0,
            solid_edges:Boolean     = property("field.solid_edges", true)) = {
    new ScageTracer[Trace](field_from_x,field_to_x,field_from_y,field_to_y,init_h_x,init_h_y,init_N_x,init_N_y,solid_edges) {
      def addTrace(point:Vec):Trace = {addTrace(point, Trace())}
    }
  }

  // maybe some other name for this factory method (like 'newTracer', etc)
  def create[T <: TraceTrait](field_from_x:Int   = property("field.from.x", 0),
                         field_to_x:Int          = property("field.to.x", try {RendererLib.windowWidth} catch {case e:Exception => 800}),
                         field_from_y:Int        = property("field.from.y", 0),
                         field_to_y:Int          = property("field.to.y", try {RendererLib.windowHeight} catch {case e:Exception => 600}),
                         init_h_x:Int            = property("field.h_x", 0),
                         init_h_y:Int            = property("field.h_y", 0),
                         init_N_x:Int            = if(property("field.h_x", 0) == 0) property("field.N_x", (try {RendererLib.windowWidth} catch {case e:Exception => 800})/50) else 0,
                         init_N_y:Int            = if(property("field.h_y", 0) == 0) property("field.N_y", (try {RendererLib.windowHeight} catch {case e:Exception => 600})/50) else 0,
                         solid_edges:Boolean     = property("field.solid_edges", true)) = {
    new ScageTracer[T](field_from_x,field_to_x,field_from_y,field_to_y,init_h_x,init_h_y,init_N_x,init_N_y,solid_edges)
  }
}