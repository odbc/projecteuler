package lib.algorithms.graphs

case class FordBellman(graph: Graph) {

  def minFrom(v: Int): Vector[BigInt] = {
    val infinity = BigInt(10).pow(100)
    val paths = Vector.fill(graph.vCount)(infinity).updated(v, BigInt(0))

    (0 until paths.length - 1).foldLeft(paths) { case (stepPaths, _) =>
      graph.edges.foldLeft(stepPaths) { case (curPaths, edge) =>
        if (curPaths(edge.source) < infinity)
          curPaths.updated(edge.target, List(curPaths(edge.target), curPaths(edge.source) + edge.weight).min)
        else curPaths
      }
    }
  }
}
