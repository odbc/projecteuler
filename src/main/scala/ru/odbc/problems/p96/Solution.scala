package ru.odbc.problems.p96

import scala.annotation.tailrec
import scala.io.Source

object Solution extends App {

  @tailrec
  def whileLoop[T](cond: T => Boolean)(f: T => T)(value: T): T =
    if (cond(value)) whileLoop(cond)(f)(f(value)) else value

  case class Row(cells: Array[Set[Int]]) {
    def isRegular: Boolean = cells.forall(_.size == 1) && cells.map(_.head).toSet == (1 to 9).toSet

    override def toString: String =
      cells.map(cell => " " * (9 - cell.size) + cell.mkString(" ") + " " * (9 - cell.size) + " | ").mkString
  }

  case class Board(rows: Array[Row]) {
    def ==(other: Board): Boolean =
      this.rows.zip(other.rows).forall { case (lr, rr) =>
        lr.cells.zip(rr.cells).forall { case (l, r) => l == r }
      }

    def !=(other: Board): Boolean = ! (this == other)

    def transpose: Board = Board {
      (for {
        i <- this.rows.indices
      } yield Row(this.rows.map { row => row.cells(i) })).toArray
    }

    def squaresToRows: Board = Board {
      (for {
        i <- this.rows.indices by 3
        j <- this.rows.indices by 3
      } yield Row {
        this.rows(i).cells.slice(j, j + 3) ++
          this.rows(i + 1).cells.slice(j, j + 3) ++
          this.rows(i + 2).cells.slice(j, j + 3)
      }).toArray
    }

    def rowsToSquares: Board = Board {
      this.rows.sliding(3, 3).flatMap { threeRows =>
        Array(
          Row(threeRows(0).cells.slice(0, 3) ++ threeRows(1).cells.slice(0, 3) ++ threeRows(2).cells.slice(0, 3)),
          Row(threeRows(0).cells.slice(3, 6) ++ threeRows(1).cells.slice(3, 6) ++ threeRows(2).cells.slice(3, 6)),
          Row(threeRows(0).cells.slice(6, 9) ++ threeRows(1).cells.slice(6, 9) ++ threeRows(2).cells.slice(6, 9)),
        )
      }.toArray
    }

    override def toString: String = rows.map(row => row.toString + "\n").mkString

    def isSolved: Boolean =
      this.rows.forall(_.isRegular) &&
        this.transpose.rows.forall(_.isRegular) &&
        this.squaresToRows.rows.forall(_.isRegular)

    def isNotSolvable: Boolean =
      this.rows.forall(_.cells.exists(_.isEmpty))
  }

  val boards = Source.fromResource("p096_sudoku.txt").getLines
    .sliding(10, 10)
    .map { grid =>
      Board {
        grid.tail.map { row =>
          Row {
            row.map { char =>
              val n = char.asDigit
              if (n == 0) (1 to 9).toSet else Set(n)
            }.toArray
          }
        }.toArray
      }
    }.toList

  def simplifyRows: Board => Board = board => Board {
    board.rows.map { row =>
      val guessed = row.cells.filter(_.size == 1).map(_.head).toSet
      val updated = Row { row.cells.map { cell => if (cell.size == 1) cell else cell diff guessed } }

      val newRow = Row {
        (1 to 9).foldLeft(updated.cells) { case (r, n) =>
          if (r.count(_.contains(n)) == 1) r.map { s => if (s.contains(n)) Set(n) else s }
          else r
        }
      }

      val newGuessed = row.cells.filter(_.size == 1).map(_.head)
      if (newGuessed.distinct.length != newGuessed.length) Row(Array.fill(9)(Set[Int]()))
      else newRow
    }
  }

  def simplifyBoard: Board => Board = board => {
    def simplifyTransposed: Board => Board = b => simplifyRows(b.transpose).transpose
    def simplifySquared: Board => Board = b => simplifyRows(b.squaresToRows).rowsToSquares
    (simplifyRows andThen simplifyTransposed andThen simplifySquared)(board)
  }

  def solve: Board => Board = board =>
    whileLoop[(Board, Board)]{ case (pred, curr) =>
      pred != curr && !curr.isSolved && !curr.isNotSolvable
    } { case (_, curr) =>
      (curr, simplifyBoard(curr))
    } {
      (Board((1 to 9).map(_ => Row(Array.fill(9)(Set(0)))).toArray), board)
    }._2

  val result = boards.map { board =>
    whileLoop[List[Board]] { _.forall(!_.isSolved) } { l =>
      val attemption = l.map(solve)
      if (attemption.exists(_.isSolved)) attemption
      else attemption.flatMap { b =>
        val (row, cell) = (for {
          s <- 2 to 9
          i <- b.rows.indices
          j <- b.rows.indices
          if b.rows(i).cells(j).size == s
        } yield (i, j)).head

        val minSet = b.rows(row).cells(cell).toVector

        minSet.map { n =>
          Board {
            b.rows.zipWithIndex.map { case (r, ri) =>
              Row {
                if (ri == row) r.cells.zipWithIndex.map { case (s, ci) => if (ci == cell) Set(n) else s }
                else r.cells
              }
            }
          }
        }
      }
    } { board :: Nil }.find(_.isSolved).get
  }

  println(result.map(_.rows.head.cells.slice(0, 3).map(_.head).mkString.toInt).sum)
}
