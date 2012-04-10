package net.scage.support.physics.objects

import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import net.phys2d.math.Vector2f
import net.phys2d.raw.shapes.Polygon
import net.phys2d.raw.StaticBody
import _root_.net.scage.support.ScageProperties._

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