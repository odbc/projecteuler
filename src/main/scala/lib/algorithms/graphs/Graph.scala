package lib.algorithms.graphs

case class Graph(vCount: Int = 0, edges: Vector[Edge] = Vector()) {

  def :+(e: Edge): Graph =
    if (e.source >= vCount || e.target >= vCount)
      this.copy(vCount = Vector(e.source, e.target).max + 1, edges = edges :+ e)
    else this.copy(edges = edges :+ e)
}

object Graph {
  def apply(edges: Vector[Edge]): Graph = Graph(edges.flatMap(e => Vector(e.source, e.target)).max + 1, edges)
}
