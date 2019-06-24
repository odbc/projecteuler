package ru.odbc.problems.p1to50.p24

object Solution extends App {

  val result = (for {
    p0 <- (0 to 9).toStream
    p1 <- (0 to 9).filter(_ != p0).toStream
    p2 <- (0 to 9).filter(n => n != p0 && n != p1).toStream
    p3 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2).toStream
    p4 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2 && n != p3).toStream
    p5 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2 && n != p3 && n != p4).toStream
    p6 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2 && n != p3 && n != p4 && n != p5).toStream
    p7 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2 && n != p3 && n != p4 && n != p5 && n != p6).toStream
    p8 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2 && n != p3 && n != p4 && n != p5 && n != p6 && n != p7).toStream
    p9 <- (0 to 9).filter(n => n != p0 && n != p1 && n != p2 && n != p3 && n != p4 && n != p5 && n != p6 && n != p7 && n != p8).toStream
  } yield Vector(p0, p1, p2, p3, p4, p5, p6, p7, p8, p9)).drop(999999)

  println(result.head.mkString)

}
