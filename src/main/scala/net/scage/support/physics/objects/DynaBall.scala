package net.scage.support.physics.objects

import net.phys2d.raw.Body
import _root_.net.scage.support.Vec
import net.phys2d.raw.shapes.Circle
import net.scage.support.physics.Physical
import _root_.net.scage.support.ScageProperties._
import java.lang.Float

class DynaBall(init_coord:Vec, val radius:Int, mass:Float = 1, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  val body = new Body(new Circle(radius), mass)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(init_coord.x, init_coord.y)

  def points = Array(coord)
}