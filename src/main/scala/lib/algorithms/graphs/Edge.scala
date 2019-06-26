package lib.algorithms.graphs

case class Edge(source: Int, target: Int, weight: BigInt = 0)

object Edge {
  def apply(pair: (Int, Int)): Edge = Edge(pair._1, pair._2)
  def apply(triple: (Int, Int, BigInt)): Edge = Edge(triple._1, triple._2, triple._3)
}
