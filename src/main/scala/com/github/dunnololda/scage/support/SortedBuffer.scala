package com.github.dunnololda.scage.support

import com.github.dunnololda.scage.ScageOperation

import scala.collection.generic.{Growable, Shrinkable}
import scala.collection.{Set, mutable}

class SortedBuffer(init_arr:ScageOperation*) extends Iterable[ScageOperation] with Growable[ScageOperation] with Shrinkable[ScageOperation] {
  private val m = new java.util.TreeMap[Int, mutable.HashMap[Int, ScageOperation]]
  private val position_by_op_id = mutable.HashMap[Int, Int]()
  private var _length = 0

  def operationIdsSet:Set[Int] = position_by_op_id.keySet
  def operationIdsSeq:Seq[Int] = position_by_op_id.keys.toSeq
  def operationIdsList:List[Int] = position_by_op_id.keys.toList
  def operationIdsIterable:Iterable[Int] = position_by_op_id.keys

  init_arr.foreach(elem => {
    this += elem
  })

  def +=(elem: ScageOperation): this.type = {
    val a = m.get(elem.position)
    if(a == null) {
      m.put(elem.position, mutable.HashMap[Int, ScageOperation](elem.op_id -> elem))
    } else {
      a += elem.op_id -> elem
    }
    position_by_op_id += elem.op_id -> elem.position
    _length += 1
    this
  }

  def clear(): Unit = {
    m.clear()
    position_by_op_id.clear()
    _length = 0
  }

  def remove(op_id: Int): Option[ScageOperation] = {
    position_by_op_id.get(op_id) match {
      case Some(position) =>
        val a = m.get(position)
        if(a != null) {
          val ans = a.remove(op_id)
          position_by_op_id -= op_id
          if(a.isEmpty) {
            m.remove(position)
          }
          _length -= 1
          ans
        } else None
      case None =>
        None
    }
  }

  def -=(elem: ScageOperation): this.type = {
    this.remove(elem.op_id)
    this
  }

  def iterator: Iterator[ScageOperation] = if(m.isEmpty) {
    new Iterator[ScageOperation] {
      def hasNext: Boolean = false
      def next(): ScageOperation = throw new NoSuchElementException("next on empty iterator")
    }
  } else {
    new Iterator[ScageOperation] {
      private val buffers_iterator = m.values().iterator()
      private var current_buffer_iterator = buffers_iterator.next().iterator

      def hasNext: Boolean = current_buffer_iterator.hasNext || buffers_iterator.hasNext

      def next(): ScageOperation = {
        if(current_buffer_iterator.hasNext) {
          current_buffer_iterator.next()._2
        } else {
          current_buffer_iterator = buffers_iterator.next().iterator
          current_buffer_iterator.next()._2
        }
      }
    }
  }

  def length: Int = _length
}

object SortedBuffer {
  def apply(init_arr:ScageOperation*) = new SortedBuffer(init_arr:_*)
}