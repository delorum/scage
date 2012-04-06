package net.scage.support.physics.objects

import net.phys2d.raw.shapes.Line
import net.phys2d.math.Vector2f
import net.phys2d.raw.StaticBody
import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import _root_.net.scage.support.ScageProperties._

class StaticLine(start:Vec, end:Vec, restitution:Boolean = property("physics.restitution", false)) extends Physical {
  val line = new Line((end-start).x, (end-start).y)

  val body = new StaticBody("line", line)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(start.x, start.y)

  def points = {
    val verts:Array[Vector2f] = line.getVertices(body.getPosition, body.getRotation);
    for(v <- verts) yield Vec(v.getX, v.getY)
  }

  /*def render() {
    val verts:Array[Vector2f] = line.getVertices(body.getPosition(), body.getRotation());
    Renderer.currentColor = WHITE
    Renderer.drawLine(Vec(verts(0).getX, verts(0).getY),
                      Vec(verts(1).getX, verts(1).getY))
  }*/
}