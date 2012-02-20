package net.scage.support.physics.objects

import net.phys2d.raw.StaticBody
import net.phys2d.raw.shapes.Box
import _root_.net.scage.support.Vec
import net.scage.support.physics.Physical
import _root_.net.scage.support.ScageProperties._

class StaticBox(init_coord:Vec, val box_width:Float, val box_height:Float, restitution:Boolean = property("physics.restitution", false)) extends Physical {
  val box = new Box(box_width, box_height)

  val body = new StaticBody("StaticBox", box)
  if(restitution) body.setRestitution(1.0f)
  body.setPosition(init_coord.x, init_coord.y)

  def points = {
    val verts = box.getPoints(body.getPosition, body.getRotation);
    for(v <- verts) yield Vec(v.getX, v.getY)
  }

  /*def render() {
    val verts:Array[Vector2f] = box.getPoints(body.getPosition(), body.getRotation());
    Renderer.currentColor = WHITE
    GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glBegin(GL11.GL_LINE_LOOP);
        verts.foreach(v => GL11.glVertex2f(v.getX, v.getY))
      GL11.glEnd();
    GL11.glEnable(GL11.GL_TEXTURE_2D);
  }*/
}