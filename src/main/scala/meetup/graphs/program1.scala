package meetup.graphs

import meetup.graphs.console._
import shapeless._

import scala.io.StdIn

object program1 extends App {
  type Program1 = readLine >> putLine

  case class |*|[x, run](run: run)


  implicit def readLineProg: readLine |*| (Unit => String) = |*|(_ => StdIn.readLine("input some string\n"))
  implicit def writeLineProg: putLine |*| (String => Unit) = |*|(println(_))
  implicit def compose[x, y, A, B, C](implicit x: x |*| (A => B), y: y |*| (B => C)): (x >> y) |*| (A => C) = |*|(a => y.run(x.run(a)))

  the[|*|[Program1, Unit => Unit]].run()

  type Program2 = readLine >> putLine >> readLine >> putLine

  the[|*|[Program2, Unit => Unit]].run()
}
