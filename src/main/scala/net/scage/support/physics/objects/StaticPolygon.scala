package net.scage.support.physics.objects

import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import net.phys2d.math.Vector2f
import net.phys2d.raw.shapes.Polygon
import net.phys2d.raw.StaticBody
import _root_.net.scage.support.ScageProperties._

class StaticPolygon(restitution:Boolean, val vertices:Vec*) extends Physical {
  def this(vertices:Vec*) {this(property("physics.restitution", false), vertices:_*)}
  val polygon_vertices = for {
    vertice <- vertices
    new_vertice = vertice - vertices(0)
  } yield new Vector2f(new_vertice.x, new_vertice.y)
  val polygon = new Polygon(polygon_vertices.toArray)
  val body = new StaticBody("StaticPolygon", polygon)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(vertices(0).x, vertices(0).y)

  private val vertices_array = vertices.toArray
  def points = vertices_array
}