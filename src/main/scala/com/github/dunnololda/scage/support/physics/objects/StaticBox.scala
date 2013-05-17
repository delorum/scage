package com.github.dunnololda.scage.support.physics.objects

import net.phys2d.raw.StaticBody
import net.phys2d.raw.shapes.Box
import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.scage.support.physics.Physical
import com.github.dunnololda.cli.AppProperties._

class StaticBox(init_coord:Vec, val box_width:Float, val box_height:Float, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  val box = new Box(box_width, box_height)

  val body = new StaticBody("StaticBox", box)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(init_coord.x, init_coord.y)

  def points = box.getPoints(body.getPosition, body.getRotation).map(v => Vec(v.getX, v.getY))
}