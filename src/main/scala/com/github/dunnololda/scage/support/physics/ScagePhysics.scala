package com.github.dunnololda.scage.support.physics

import net.phys2d.raw.World
import com.github.dunnololda.cli.AppProperties._
import net.phys2d.math.Vector2f
import net.phys2d.raw.strategies.QuadSpaceStrategy
import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.mysimplelogger.MySimpleLogger
import collection.mutable

object ScagePhysics {
  def str2vec(s:String):Vec = {
    val coords = s.replace("[", "").replace("]", "").split(",")
    try {
      Vec(coords(0).trim.toFloat, coords(1).trim.toFloat)
    } catch {
      case e:Exception => Vec.zero
    }
  }

  def apply(dt:Int = property("physics.dt", 5),
            gravity:Vec = str2vec(property("physics.gravity", "[0, 0]"))) = {
    new ScagePhysics(dt, gravity)
  }
}

class ScagePhysics(
  val dt:Int = property("physics.dt", 5),   // see exactly no purposes to make it changeable. If I find any - I will do it.
  val gravity:Vec = ScagePhysics.str2vec(property("physics.gravity", "[0, 0]"))
) {
  private val log = MySimpleLogger(this.getClass.getName)

  /*def dt = _dt
  def dt_=(new_dt:Int) {
    if(new_dt > 0) dt = new_dt
    else log.error("failed to update dt: must be more then zero but the value is "+new_dt)
  }*/

  val world = new World(new Vector2f(gravity.x, gravity.y), 10, new QuadSpaceStrategy(20,10))
  world.enableRestingBodyDetection(0.01f, 0.000001f, 0.01f)
  
  private val _physicals = mutable.HashSet[Physical]()
  def physicals = _physicals.toList
  def addPhysical(physical:Physical) = {
    if(!world.getBodies.contains(physical.body)) world.add(physical.body)
    _physicals += physical
    physical.clearTouches()
    log.debug("added new physical "+physical)
    physical
  }
  def addPhysicals(physicals:Physical*) {
    physicals.foreach(addPhysical)
  }

  // TODO: мб запилить по аналогии removePhysical, возвращающий, кого удалил.
  // TODO: И метод, принимающий условие в качестве параметра. И все такое
  def removePhysicals(physicals_to_delete:Physical*) {
    for(p <- physicals_to_delete) {
      if(_physicals.contains(p)) {
        world.remove(p.body)
        _physicals -= p
        log.debug("removed physical "+p)
      } else {
        log.warn("physical "+p+" not found")
      }
    }
  }
  def removeAll() {
    world.clear()
    _physicals.clear()
    log.info("deleted all physical objects")
  }

  def containsPhysical(p:Physical) = _physicals.contains(p)

  def step() {
    _physicals.foreach(_.clearTouches())

    for(i <- 1 to dt) {
      world.step()
      for(p <- _physicals) {
        p.updateCollisions(world.getContacts(p.body))
      }
    }
  }

  def touchingPoints(p:Physical) = {
    (for(ce <- world.getContacts(p.body)) yield {
      val phys2d_point= ce.getPoint
      val phys2d_normal = ce.getNormal
      (new Vec(phys2d_point), new Vec(phys2d_normal))
    }).toList
  }
}