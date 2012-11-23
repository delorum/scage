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

object SortedBuffer {
  def apply[A <: Ordered[A]](init_arr:A*) = new SortedBuffer(init_arr:_*)
}