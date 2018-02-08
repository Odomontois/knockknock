package meetup.graphs

import cats._
import cats.data.WriterT
import meetup.control.{RCons, RConsF, RNilF, ValueF}
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
    import Monadic.Ev.instance



    type NameDict =  RConsF[Id, Name, ValueF[Id, String], RNilF[Id]]
    InterpretF.interpretMacro[readLine[Name], Id, RNilF[Id]]

    Monadic.putLineProg[Name, Id, Wrtr]
//    Monadic.varInputProg[putLine[Name], Name, Id, String, NameDict, Unit]
//    InterpretF.interpretMacro[putLine[Name] <<- Name, Id, NameDict]

//    InterpretF.interpretMacro[Monadic.Aux, Program2[Name], Id, RNilF[Id]]
//    InterpretF.interpretMacro[Monadic.Aux, do_[putLine[Name] <<- Name], Rdr, RCons[Name, String, RNil]]
    //    InterpretF.interpretMacro[Monadic.Aux, readVar[Name], Id, RNil]
    //    Monadic.varOutputProg[readLine[Name], Id, RNil, Name,  ReaderT[Id, UserInput,?], String, RCons[Name, String, RNil]]
    val input = Map(
      "name" -> "Oleg",
      "age" -> "32",
      "first name" -> "Oleg",
      "last name" -> "Nizhnik"
    )
    type Rdr[a] = ReaderT[Id, UserInput, a]
    type Wrtr[a] = WriterT[Rdr, UserOutput, a]
//    Monadic.varOutputProg[readLine[Name], Id, RNil, Name, String]
//    implicitly[Monadic[readLine[Name], Id, RNil]]
//    implicitly[Monadic[putVar[Name], Rdr, RCons[Name, String, RNil]]]
//    implicitly[Monadic[putLine[Name], Rdr, String]]
//    implicitly[Monadic[putLine[Name], Rdr, String]]
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

    println((3 match {
//      case 0 => withTypeTag[Program1A[Name]].run.run(input)
//      case 1 => withTypeTag[Program1[Name]].run.run(input)
//      case 2 => withDisplayOut[Program2[Name]].run.run(input)
//      case 3 => withDisplayOut[Program3[Name, Age]].run.run(input)
//      case 4 => withDisplayOut[Program4].run.run(input)
      //       case 5 => withDisplayOut[Program5].run()
      //       case 6 => withDisplayOut[Program6].run()
      case _ =>
      case _ => null
    }) match {
      case (x, y) => s"Written: \n$x\nValue: \n$y"
      case x => x
    })
  }
}
