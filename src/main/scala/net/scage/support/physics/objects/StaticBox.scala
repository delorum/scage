package net.scage.support.physics.objects

import net.phys2d.raw.StaticBody
import net.phys2d.raw.shapes.Box
import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import _root_.net.scage.support.ScageProperties._

class StaticBox(init_coord:Vec, val box_width:Float, val box_height:Float, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  val box = new Box(box_width, box_height)

  val body = new StaticBody("StaticBox", box)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(init_coord.x, init_coord.y)

  def points = box.getPoints(body.getPosition, body.getRotation).map(v => Vec(v.getX, v.getY))
}