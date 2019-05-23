package ru.odbc.problems.p96

import scala.annotation.tailrec
import scala.io.Source

object Solution extends App {

  type Board = Array[Array[Set[Int]]]

  def isBoardsEqual: (Board, Board) => Boolean =
    (left, right) =>
      (for {
        i <- left.indices
        j <- left.indices
      } yield (i, j)).forall { case (i, j) => left(i)(j) == right(i)(j) }

  def isBoardSolved: Board => Boolean = board => board.forall(_.forall(_.size == 1))

  def transpose: Board => Board =
    board => (for { i <- board.indices } yield board.map { row => row(i) }).toArray

  def squaresToRows: Board => Board = board =>
    (for {
      i <- board.indices by 3
      j <- board.indices by 3
    } yield board(i).slice(j, j + 3) ++ board(i + 1).slice(j, j + 3) ++ board(i + 2).slice(j, j + 3)).toArray

  def rowsToSquares: Board => Board = board =>
    board.sliding(3, 3).flatMap { threeRows =>
      Array(
        threeRows(0).slice(0, 3) ++ threeRows(1).slice(0, 3) ++ threeRows(2).slice(0, 3),
        threeRows(0).slice(3, 6) ++ threeRows(1).slice(3, 6) ++ threeRows(2).slice(3, 6),
        threeRows(0).slice(6, 9) ++ threeRows(1).slice(6, 9) ++ threeRows(2).slice(6, 9),
      )
    }.toArray

  def updateRows: Board => Board = board => {
    val lll = board.map { row =>
      val solved = row.filter(_.size == 1).map(_.head).toSet
      val updated = row.map { cell => if (cell.size == 1) cell else cell diff solved }

      (1 to 9).foldLeft(updated) { case (r, n) =>
        if (r.count(_.contains(n)) == 1) r.map { s => if (s.contains(n)) Set(n) else s }
        else r
      }
    }
    if (lll(0)(5) == Set(4)) lll.foreach(row => println(row.mkString(" ")))
    lll
  }

  def updateBoard: Board => Board = board =>
    (updateRows andThen
      transpose andThen updateRows andThen transpose andThen
      squaresToRows andThen updateRows andThen rowsToSquares)(board)

  def solve: Board => Board = board =>
    whileLoop[(Board, Board)] { case (pred, curr) =>
      !isBoardsEqual(pred, curr) && !isBoardSolved(curr)
    } { case (_, curr) =>
      //println(curr.map(_.toVector.map(_.toList)).toVector)
      //curr.foreach(r => println(r.map(s => if (s.size == 1) s.head else 0).mkString(" ")))
      (curr, updateBoard(curr))
    } { (Array.fill(9, 9)(Set(0)), board) }._2

  @tailrec
  def whileLoop[T](cond: T => Boolean)(f: T => T)(value: T): T =
    if (cond(value)) whileLoop(cond)(f)(f(value)) else value

  val boards = Source.fromResource("p096_sudoku.txt").getLines
    .sliding(10, 10)
    .map(l => l.tail.map(s => s.map(c => { val n = c.asDigit; if (n == 0) (1 to 9).toSet else Set(n) }).toArray).toArray)
    .toList

  val result = List(boards(9)).map { board =>
    whileLoop[List[Board]] { _.forall(!isBoardSolved(_)) } { l =>
      val attemption = l.map(solve)
      attemption.foreach(_.foreach(row => println(row.mkString(" "))))
      if (attemption.exists(isBoardSolved)) attemption
      else attemption.flatMap { b =>
        val pi = (for {
          i <- b.indices
          j <- b.indices
          if b(i)(j).size == 2
        } yield (i, j)).head

        val pair = b(pi._1)(pi._2).toList
        println(pair)

        val lll = List(
          b.zipWithIndex.map { case (r, ri) =>
            if (ri == pi._1) r.zipWithIndex.map { case (s, ci) => if (ci == pi._2) Set(pair.head) else s } else r
          },
          b.zipWithIndex.map { case (r, ri) =>
            if (ri == pi._1) r.zipWithIndex.map { case (s, ci) => if (ci == pi._2) Set(pair.last) else s } else r
          }
        )
        println(lll.foreach(ppp => if (ppp(0)(5) == Set(4)) ppp.foreach(row => println(row.mkString(" ")))))
        lll
      }
    } { board :: Nil }.find(isBoardSolved).get.map(_.map(_.head))
  }

  result.foreach(b => { b.foreach(row => println(row.mkString(" "))); println(b.head.slice(0, 3).mkString) })

  result.find { b =>
    b.exists( row => row.toSet != (1 to 9).toSet )
  }.get.foreach(row => println(row.mkString(" ")))

  println(result.map(_.head.slice(0, 3).mkString.toInt).sum)
}
