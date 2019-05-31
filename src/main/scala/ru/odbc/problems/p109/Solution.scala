package ru.odbc.problems.p109

object Solution extends App {

  sealed trait Shot { def score: Int }

  sealed trait Hit extends Shot { def section: Int }
  case class  SingleHit(section: Int) extends Hit { override def score: Int = section }
  case class  DoubleHit(section: Int) extends Hit { override def score: Int = 2 * section }
  case class  TripleHit(section: Int) extends Hit { override def score: Int = 3 * section }

  case object MissedShot extends Shot { override def score: Int = 0 }

  case class ThreeDarts(first: Shot, second: Shot, third: Shot) {
    def ===(other: ThreeDarts): Boolean =
      this.third == other.third &&
        ((this.first == MissedShot && other.first == MissedShot && this.second == other.second) ||
          (this.first == other.first && this.second == other.second) ||
          (this.first == other.second && this.second == other.first))

    def score: Int = first.score + second.score + third.score
  }

  val sections = 25 :: (1 to 20).toList

  val firstShot  = sections.map(s => ThreeDarts(MissedShot, MissedShot, DoubleHit(s)))
  val secondShot = for {
    s  <- sections
    fs <-
      if (s == 25) List(SingleHit(s), DoubleHit(s))
      else List(SingleHit(s), DoubleHit(s), TripleHit(s))
    ss <- firstShot
  } yield ThreeDarts(MissedShot, fs, ss.third)

  val thirdShot = for {
    s  <- sections
    fs <-
      if (s == 25) List(SingleHit(s), DoubleHit(s))
      else List(SingleHit(s), DoubleHit(s), TripleHit(s))
    ss <- fs match {
      case SingleHit(shv) => secondShot.collect {
        case ts@ThreeDarts(_, SingleHit(shvr), _) if shv <= shvr => ts
        case tdtt@ThreeDarts(_, _: DoubleHit | _: TripleHit, _)   => tdtt
      }
      case DoubleHit(dhv) => secondShot.collect {
        case td@ThreeDarts(_, DoubleHit(dhvr), _) if dhv <= dhvr => td
        case ttt@ThreeDarts(_, _: TripleHit, _)                  => ttt
      }
      case TripleHit(thv) => secondShot.collect {
        case tt@ThreeDarts(_, TripleHit(thvr), _) if thv <= thvr => tt
      }
    }
  } yield ThreeDarts(fs, ss.second, ss.third)

  val checkouts = firstShot ++ secondShot ++ thirdShot

  val result = checkouts.filter(_.score < 100)

  println(result.size)
}
