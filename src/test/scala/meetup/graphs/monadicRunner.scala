package meetup.graphs

import cats._
//import cats.implicits._
import cats.data.ReaderT
import meetup.control.Record.RNil
import meetup.graphs.Monadic.runners._
import meetup.graphs.prog._
import cats.instances.vector._

object monadicRunner {
  def main(args: Array[String]): Unit = {
  Interpret.interpretMacro[Monadic.Aux, Program1[Name], Base]
     println(1 match {
       case 0 => withTypeTag[Program1A[Name]].run.x.run(Map("name" -> "Oleg"))
       case 1 => withTypeTag[Program1[Name]].run.x.run(Map("name" -> "Oleg"))
//       case 2 => withDisplayOut[Program2[Name]].run()
//       case 3 => withDisplayOut[Program3["name", "age"]].run()
//       case 4 => withDisplayOut[Program4].run()
//       case 5 => withDisplayOut[Program5].run()
//       case 6 => withDisplayOut[Program6].run()
     })
  }
}
