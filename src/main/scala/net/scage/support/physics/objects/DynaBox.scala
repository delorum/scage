package net.scage.support.physics.objects

import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import net.phys2d.raw.Body
import net.phys2d.raw.shapes.Box
import java.lang.Float
import _root_.net.scage.support.ScageProperties._

class DynaBox(init_coord:Vec, val box_width:Float, val box_height:Float, val box_mass:Float = 1, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  val box = new Box(box_width, box_height)
  val body = new Body(box, box_mass)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(init_coord.x, init_coord.y)

  def points = box.getPoints(body.getPosition, body.getRotation).map(v => Vec(v.getX, v.getY))
}