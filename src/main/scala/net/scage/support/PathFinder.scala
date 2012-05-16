package net.scage.support

import org.newdawn.slick.util.pathfinding.{PathFindingContext, TileBasedMap, AStarPathFinder}
import collection.mutable.Stack

class PathFinder(width:Int, height:Int, val is_blocked:(Int, Int) => Boolean = (x, y) => false, val cost:(Int, Int) => Float = (x, y) => 1f) {
  private val slick_astar_path_finder = new AStarPathFinder(new TileBasedMap {
    def getWidthInTiles = width
    def getHeightInTiles = height
    def pathFinderVisited(x:Int, y:Int) {}
    def blocked(context:PathFindingContext, tx:Int, ty:Int) = is_blocked(tx, ty)
    def getCost(context:PathFindingContext, tx:Int, ty:Int) = cost(tx, ty)
  }, 100500, true)

  def findPath(p1:Vec, p2:Vec) = {
    val slick_path = slick_astar_path_finder.findPath(null, p1.ix, p1.iy, p2.ix, p2.iy)
    if(slick_path != null) Stack((for(i <- 0 until slick_path.getLength) yield Vec(slick_path.getX(i), slick_path.getY(i))):_*)
    else Stack[Vec]()
  }
}

object PathFinder {
  def apply(width:Int, height:Int, is_blocked:(Int, Int) => Boolean = (x, y) => false, cost:(Int, Int) => Float = (x, y) => 1f) = {
    new PathFinder(width, height, is_blocked, cost)
  }
}
