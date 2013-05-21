package com.github.dunnololda.scage.support.physics.objects

import net.phys2d.raw.shapes.Line
import net.phys2d.raw.StaticBody
import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.scage.support.physics.Physical
import com.github.dunnololda.cli.AppProperties._

class StaticLine(start:Vec, end:Vec, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  private val line = new Line((end-start).x, (end-start).y)

  val body = new StaticBody("line", line)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(start.x, start.y)

  def points = line.getVertices(body.getPosition, body.getRotation).map(v => Vec(v.getX, v.getY))
}