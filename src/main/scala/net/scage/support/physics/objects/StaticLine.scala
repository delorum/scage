package net.scage.support.physics.objects

import net.phys2d.raw.shapes.Line
import net.phys2d.math.Vector2f
import net.phys2d.raw.StaticBody
import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import _root_.net.scage.support.ScageProperties._

class StaticLine(start:Vec, end:Vec, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  private val line = new Line((end-start).x, (end-start).y)

  val body = new StaticBody("line", line)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(start.x, start.y)

  def points = line.getVertices(body.getPosition, body.getRotation).map(v => Vec(v.getX, v.getY))
}