package net.scage.support

import collection.mutable.ArrayBuffer
import collection.generic.{Shrinkable, Growable}

class SortedBuffer[A <: Ordered[A]](init_arr:A*) extends Seq[A] with Growable[A] with Shrinkable[A] {
  private val arr = ArrayBuffer(init_arr:_*).sortWith(_ < _)

  def remove(idx:Int) = arr.remove(idx)

  def clear() {arr.clear()}

  def +=(elem:A) = {
    def pos = arr.indexWhere(elem < _)
    if(pos == -1) arr += elem
    else arr.insert(pos, elem)
    this
  }

  def -=(elem:A) = {
    arr -= elem
    this
  }

  def apply(idx: Int) = arr.apply(idx)
  def length: Int = arr.length
  def iterator: Iterator[A] = arr.iterator
}

class SynchronizedSortedBuffer[A <: Ordered[A]](init_arr:A*) extends SortedBuffer[A](init_arr:_*) {
  override def remove(idx:Int) = synchronized {super.remove(idx)}

  override def clear() {synchronized {super.clear()}}

  override def +=(elem:A) = synchronized[this.type] {
    super.+=(elem)
    this
  }

  override def -=(elem:A) = synchronized[this.type] {
    super.-=(elem)
    this
  }

  override def apply(idx: Int) = synchronized {super.apply(idx)}
  override def length: Int = synchronized {super.length}
  override def iterator: Iterator[A] = synchronized {super.iterator}
}

object SortedBuffer {
  def apply[A <: Ordered[A]](init_arr:A*) = new SortedBuffer(init_arr:_*)
}