package net.scage.support.tracer3

import net.scage.support.Vec
import net.scage.support.ScageProperties._
import com.weiglewilczek.slf4s.Logger
import collection.mutable.{ArrayBuffer, HashMap}
import net.scage.handlers.RendererLib

class ScageTracer[T <: TraceTrait](val field_from_x:Int = property("field.from.x", 0),
                              val field_to_x:Int        = property("field.to.x", try {RendererLib.windowWidth} catch {case e:Exception => 800}),
                              val field_from_y:Int      = property("field.from.y", 0),
                              val field_to_y:Int        = property("field.to.y", try {RendererLib.windowHeight} catch {case e:Exception => 600}),
                              init_h_x:Int              = property("field.h_x", 0),
                              init_h_y:Int              = property("field.h_y", 0),
                              init_N_x:Int              = if(property("field.h_x", 0) == 0) property("field.N_x", (try {RendererLib.windowWidth} catch {case e:Exception => 800})/50) else 0,
                              init_N_y:Int              = if(property("field.h_y", 0) == 0) property("field.N_y", (try {RendererLib.windowHeight} catch {case e:Exception => 800})/50) else 0,
                              val solid_edges:Boolean   = property("field.solid_edges", true)) {
  private val log = Logger(this.getClass.getName)

  val field_width = field_to_x - field_from_x
  val field_height = field_to_y - field_from_y

  val N_x = if(init_h_x != 0) field_width/init_h_x else init_N_x
  val N_y = if(init_h_y != 0) field_height/init_h_y else init_N_y

  val h_x = if(init_h_x == 0) field_width/init_N_x else init_h_x
  val h_y = if(init_h_y == 0) field_height/init_N_y else init_h_y

  log.debug("creating tracer "+this.getClass.getName+": h_x="+h_x+" h_y="+h_y+" N_x="+N_x+" N_y="+N_y)

  // for client classes - children of ScageTracer
  protected def setTraceLocation(trace:T, new_location:Vec) {trace._location = new_location}
  protected implicit def trace2updateable(trace:T) = new ScalaObject {
    def __location = trace._location
    def __location_=(new_location:Vec) {trace._location = new_location}
  }

  def isPointOnArea(point:Vec):Boolean = point.x >= 0 && point.x < N_x && point.y >= 0 && point.y < N_y
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
  protected val traces_by_ids = HashMap[Int, T]()
  protected var traces_list:ArrayBuffer[T] = ArrayBuffer[T]()
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

  def containsTrace(trace_id:Int) = traces_by_ids.contains(trace_id)
  def containsTrace(trace:T) = traces_by_ids.contains(trace.id)

  def removeTraces(traces_to_remove:T*) {  // maybe return result (true/false)
    traces_to_remove.foreach(trace => {
      if(traces_by_ids.contains(trace.id)) {
        point_matrix(trace.location.ix)(trace.location.iy) -= trace
        traces_by_ids -= trace.id
        traces_list -= trace
        log.debug("removed trace #"+trace.id)
      } else log.warn("trace #"+trace.id+" not found")
    })
  }
  def removeTracesById(trace_ids:Int*) {
    removeTraces(traces_list.filter(elem => trace_ids.contains(elem.id)):_*)
  }
  def removeAllTraces() {
    clearMatrix(point_matrix)
    traces_by_ids.clear()
    traces_list.clear()
    log.info("deleted all traces")
  }
  def removeTracesInPoint(point:Vec) {removeTraces(tracesInPoint(point):_*)}

  def tracesInPoint(point:Vec, condition:T => Boolean) = {
    if(!isPointOnArea(point)) Nil
    else {
      for {
        trace <- point_matrix(point.ix)(point.iy)
        if condition(trace)
      } yield trace
    }
  }
  def tracesInPoint(point:Vec) = {
    if(!isPointOnArea(point)) Nil
    else (point_matrix(point.ix)(point.iy)).toList
  }

  def tracesInPointRange(xrange:Range, yrange:Range, condition:T => Boolean):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, yrange, condition)
  def tracesInPointRange(xrange:Range, condition:T => Boolean):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, xrange, condition)
  def tracesInPointRange(xrange:Range, yrange:Range):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, yrange)
  def tracesInPointRange(xrange:Range):IndexedSeq[T] = tracesNearPoint(Vec.zero, xrange, xrange)

  def tracesNearPoint(point:Vec, xrange:Range, yrange:Range, condition:T => Boolean):IndexedSeq[T] = {
    (for {
      i <- xrange
      j <- yrange
      near_point = outsidePoint(point + Vec(i, j))
      if isPointOnArea(near_point)
      trace <- tracesInPoint(near_point, condition)
    } yield trace)
  }
  def tracesNearPoint(point:Vec, xrange:Range, condition:T => Boolean):IndexedSeq[T] = tracesNearPoint(point, xrange, xrange, condition)
  def tracesNearPoint(point:Vec, xrange:Range, yrange:Range):IndexedSeq[T] = {
    (for {
      i <- xrange
      j <- yrange
      near_point = outsidePoint(point + Vec(i, j))
      if isPointOnArea(near_point)
      trace <- tracesInPoint(near_point)
    } yield trace)
  }
  def tracesNearPoint(point:Vec, xrange:Range):IndexedSeq[T] = tracesNearPoint(point, xrange, xrange)

  val LOCATION_UPDATED = 0
  val SAME_LOCATION    = 1
  val OUT_OF_AREA      = 2
  val TRACE_NOT_FOUND  = 3
  def updateLocation(trace_id:Int, new_point:Vec):Int = { // TODO: maybe return tuple (new_location, operation_status), maybe rename
    traces_by_ids.get(trace_id) match {
      case Some(updateable_trace) => {
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
      }
      case None => {
        log.warn("trace with id "+trace_id+" not found")
        TRACE_NOT_FOUND
      }
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
    for(i <- 0 until num_tries) {
      log.debug("generating random point, try "+i)
      val point = randomPoint()
      if(condition(point)) return Some(point)
    }      
    return None  
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
    for(i <- 0 until num_tries) {
      log.debug("generating random coord, try "+i)
      val coord = randomCoord()
      if(condition(coord)) return Some(coord)
    }      
    return None
  }

  def traceGrid = {
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