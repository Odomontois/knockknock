package meetup.graphs

import meetup.graphs.console._

import scala.io.StdIn

object programBase extends App {
  type Program1 = readLine[Unit] >> putLine[Unit]

  case class |*|[x, run](run: run)


  implicit def readLineProg[name]: readLine[name] |*| (Unit => String) = |*|(_ => StdIn.readLine("input some string\n"))
  implicit def writeLineProg[name]: putLine[Unit] |*| (String => Unit) = |*|(println(_))
  implicit def compose[x, y, A, B, C](implicit x: x |*| (A => B), y: y |*| (B => C)): (x >> y) |*| (A => C) = |*|(a => y.run(x.run(a)))

//  implicitly[|*|[Program1, Unit => Unit]].run(())

  type Program2 = readLine[Unit] >> putLine[Unit] >> readLine[Unit] >> putLine[Unit]

  implicitly[|*|[Program2, Unit => Unit]].run(())
}
