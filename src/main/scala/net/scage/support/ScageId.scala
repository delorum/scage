package net.scage.support

object ScageId {  // TODO: add some id rotation algorithm or throw error and exit on id amount exceeded
  protected var id = 10000
  def nextId = {
    synchronized  {
      id += 1
      id
    }
  }
}