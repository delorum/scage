package net.scage.support.physics

import net.phys2d.math.Vector2f
import _root_.net.scage.support.Vec
import net.phys2d.raw.{CollisionEvent, Body, BodyList}
import collection.mutable.ArrayBuffer

trait Physical {
  def body:Body

  def addForce(force:Vec) {
    body.setIsResting(false)
    body.addForce(new Vector2f(force.x, force.y))
  }

  def coord = {
    val pos = body.getPosition
    Vec(pos.getX, pos.getY)
  }
  def coord_=(new_coord:Vec) {
    body.move(new_coord.x, new_coord.y)
  }
  def move(delta:Vec) {
    val new_coord = coord + delta
    body.move(new_coord.x, new_coord.y)
  }

  def velocity = {
    val vel = body.getVelocity
    Vec(vel.getX, vel.getY)
  }
  def velocity_=(new_velocity:Vec) {
    val delta = new_velocity - velocity
    body.adjustVelocity(new Vector2f(delta.x, delta.y))
  }

  private var is_touching = false
  private val touching_bodies = new BodyList
  private val touching_points = ArrayBuffer[(Vec, Vec)]()
  def isTouching = is_touching
  def isTouching(p:Physical) = touching_bodies.contains(p.body)

  private[physics] def clearTouches() {   // will remove private modifier if needed
    is_touching = false
    touching_bodies.clear()
    touching_points.clear()
  }

  private[physics] def updateCollisions(collisions:Array[CollisionEvent]) {
    val new_touching_bodies =  body.getTouching
    is_touching = new_touching_bodies.size > 0
    if(is_touching) {
      for {
        i <- 0 until new_touching_bodies.size
        body = new_touching_bodies.get(i)
        if !touching_bodies.contains(body)
      } touching_bodies.add(body)
      for {
        ce <- collisions
        new_tp = (new Vec(ce.getPoint), new Vec(ce.getNormal))
        if !touching_points.exists(tp => tp._1 == new_tp._1 && tp._2 == new_tp._2)
      } touching_points += new_tp
    }
  }

  /*private[physics] def isTouching_=(new_is_touching:Boolean) {
    is_touching = new_is_touching
    if(is_touching) {
      val new_touching_bodies =  body.getTouching
      for {
        i <- 0 until new_touching_bodies.size
        body = new_touching_bodies.get(i)
        if !touching_bodies.contains(body)
      } touching_bodies.add(body)
    }
    else touching_bodies.clear()
  }*/

  def points:Array[Vec]

  def touchingPoints = touching_points.toList
}