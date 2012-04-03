package net.scage.support

trait ScageIdTrait {
  def nextId:Int
}

object ScageId extends ScageIdTrait {  // TODO: add some id rotation algorithm or throw error and exit on id amount exceeded
  protected var id = 10000
  def nextId = {
    synchronized  {
      id += 1
      id
    }
  }
}