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
import cats.mtl.instances.all._

import scala.reflect.runtime.universe.typeOf
object monadicRunner {
  def main(args: Array[String]): Unit = {
    import Monadic.Ev.instance
    val y = implicitly[Monadic.Ev[readLine[Name], Id, RNil, String]]
    val z = implicitly[Monadic.Ev.Aux[readLine[Name], Id, RNil, Rdr, String]]
        InterpretF.interpretMacro[Monadic.Aux, Program2[Name], Id, RNil]
//    InterpretF.interpretMacro[Monadic.Aux, readVar[Name], Id, RNil]
    //    Monadic.varOutputProg[readLine[Name], Id, RNil, Name,  ReaderT[Id, UserInput,?], String, RCons[Name, String, RNil]]
    val input = Map(
    "name" -> "Oleg",
    "first name" -> "Oleg",
    "last name" -> "Nizhnik"
    )
    type Rdr[a] = ReaderT[Id, UserInput, a]
    type Wrtr[a] = WriterT[Rdr, UserOutput, a]
    Monadic.varOutputProg[readLine[Name], Id, RNil, Name, String]
    implicitly[Monadic[readLine[Name], Id, RNil]]
    implicitly[Monadic[Name := readLine[Name], Id, RNil]]
    implicitly[Monadic[putLine[Name], Rdr, String]]
    implicitly[Monadic[putLine[Name], Rdr, String]]
    //    val ee1 = the[Eval1[readLine[Name], Id, RNil, Rdr, String]]
    //    val tt = typeOf[ee1.type].widen
    //    println(tt)
    //    implicitly[ee1.FOut[_] =:= ReaderT[Id, UserInput, _]]
    //    implicitly[ee1.VOut =:= String]
    //    implicitly[ee1.Instance =:= interpret[readLine[Name], Id, RNil,  ReaderT[Id, UserInput, ?], String]]
    //        Monadic.Eval.eval[readLine[Name], Id, RNil, interpret[readLine[Name], Id, RNil,  ReaderT[Id, UserInput, ?], String],  ReaderT[Id, UserInput,?], String]
//    implicitly[Monadic.EvalU[readLine[Name], Id, RNil]]

    //    val x: Monadic.Eval[readLine[Name], Id, RNil,  ReaderT[Id, UserInput,?], String] =
    //      Interpret.interpretMacro[Monadic.Aux, readLine[Name], Monadic.Val[Id, RNil]]

    println(1 match {
                   case 0 => withTypeTag[Program1A[Name]].run.run(input)
                   case 1 => withTypeTag[Program1[Name]].run.run(input)
//                   case 2 => withDisplayOut[Program2[Name]].run.run(input)
      //       case 3 => withDisplayOut[Program3["name", "age"]].run()
      //       case 4 => withDisplayOut[Program4].run()
      //       case 5 => withDisplayOut[Program5].run()
      //       case 6 => withDisplayOut[Program6].run()
      case _ =>
    })
  }
}
