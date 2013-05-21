package com.github.dunnololda.scage.support.physics.objects

import com.github.dunnololda.scage.support.Vec
import com.github.dunnololda.scage.support.physics.Physical
import net.phys2d.raw.shapes.Polygon
import net.phys2d.raw.StaticBody
import com.github.dunnololda.cli.AppProperties._

class StaticPolygon(restitution:Boolean, vertices:List[Vec]) extends Physical {
  def this(vertices:Vec*) {this(property("physics.restitution", true), vertices.toList)}
  def this(vertices:Array[Vec]) {this(property("physics.restitution", true), vertices.toList)}
  def this(vertices:List[Vec]) {this(property("physics.restitution", true), vertices)}

  private val polygon_vertices = for {
    vertice <- vertices
    new_vertice = vertice - vertices(0)
  } yield new_vertice
  private val polygon = new Polygon(polygon_vertices.map(_.toPhys2dVec).toArray)

  val body = new StaticBody("StaticPolygon", polygon)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(vertices(0).x, vertices(0).y)

  def points = polygon.getVertices(body.getPosition, body.getRotation).map(v => Vec(v.getX, v.getY))
}