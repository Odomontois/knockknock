package meetup.graphs

import cats._
import cats.data.WriterT
import meetup.control.RCons
import meetup.graphs.Monadic.{UserInput, UserOutput, interpret}
import meetup.graphs.console._
import meetup.graphs.dsl._
import shapeless.the
//import cats.implicits._
import cats.data.ReaderT
import meetup.control.Record.RNil
import meetup.graphs.Monadic.runners._
import meetup.graphs.prog._
import cats.instances.vector._
import cats.mtl.implicits._

import scala.reflect.runtime.universe.typeOf

object monadicRunner {
  def main(args: Array[String]): Unit = {

    val input = Map(
      "name"         -> "Oleg",
      "age"          -> "32",
      "first name"   -> "Oleg",
      "last name"    -> "Nizhnik"
    )

    println((4 match {
      case 0 => withTypeTag[Program1A[Name]].run.run(input)
      case 1 => withTypeTag[Program1[Name]].run.run(input)
      case 2 => withDisplayOut[Program2[Name]].run.run(input)
      case 3 => withDisplayOut[Program3[Name, Age]].run.run(input)
      case 4 => withDisplayOut[Program4].run.run(input)
      case _ =>
    }) match {
      case (x, y) => s"Written: \n$x\nValue: \n$y"
      case x => x
    })
  }
}
