package com.github.dunnololda.scage.support.physics.objects

import net.phys2d.raw.shapes.Circle
import net.phys2d.raw.StaticBody
import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.scage.support.physics.Physical
import com.github.dunnololda.cli.AppProperties._

class StaticBall(init_coord:Vec, val radius:Int, restitution:Boolean = property("physics.restitution", true)) extends Physical {
  val body = new StaticBody("StaticBall", new Circle(radius))
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(init_coord.x, init_coord.y)

  def points = Array(coord)
}
