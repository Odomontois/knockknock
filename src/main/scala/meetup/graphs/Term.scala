package meetup.graphs

import meetup.control._
import meetup.graphs.console._
import Record._
import meetup.graphs.Interpret.Aux
import meetup.graphs.lib.concat
import shapeless.Witness

import scala.io.StdIn

trait Term[x, Input] {
  type Output
  def run: Input => Output
  def as[y]: Term.Aux[y, Input, Output] = this.asInstanceOf[Term.Aux[y, Input, Output]]
}

object Term {
  type Aux[x, I, O] = Term[x, I] { type Output = O  }
  type Eval[x, I, O] = Interpret.Aux[Aux, x, I, O]
  type EvalU[x, I] = Interpret[Aux, x, I]


  case class interpret[x, I, O](run: I => O) extends Term[x, I] {
    type Output = O
  }

  implicit def readLineProg[name <: String, Vars <: Record](implicit name: Witness.Aux[name]): Aux[readLine[name], Vars, String] =
    interpret(_ => StdIn.readLine(s"input ${name.value}:\n"))

  implicit def putLineProg[name <: String](implicit name: Witness.Aux[name]): Aux[putLine[name], String, Unit] =
    interpret(value => println(s"[${name.value}] $value"))

  implicit def comboProg[x, y, Vars <: Record, Out1, Out2]
  (implicit x: Eval[x, Vars, Out1], y: Eval[y, Vars, Out2]): Aux[(x, y), Vars, (Out1, Out2)] =
    interpret(vars => (x.value.run(vars), y.value.run(vars)))




  implicit def varOutputProg[x, Vars <: Record, name <: String, Out, ROut <: Record]
  (implicit x: Eval[x, Vars, Out],
   update: UpdateRec.Aux[name, Vars, Out, ROut]): Aux[x ->> name, Vars, ROut] =
    interpret(vars => update(x.value.run(vars), vars))

  implicit def varInputProg[x, name <: String, In, Out, Vars <: Record]
  (implicit select: SelectRec.Aux[name, Vars, In],
   x: Eval[x, In, Out]): Aux[x <<- name, Vars, Out] =
    interpret(vars => x.value.run(select.apply(vars)))

  implicit def varInput2Prog[x, name1 <: String, name2 <: String, In1, In2, Out, Vars <: Record]
  (implicit select1: SelectRec.Aux[name1, Vars, In1],
   select2: SelectRec.Aux[name2, Vars, In2],
   x: Eval[x, (In1, In2), Out]): Aux[x <<- (name1, name2), Vars, Out] =
    interpret(vars => x.value.run((select1(vars), select2(vars))))

  implicit def getProg[name, Vars <: Record, Out](implicit select: SelectRec.Aux[name, Vars, Out]): Aux[get[name], Vars, Out] =
    interpret(vars => select(vars))



  implicit def doProg[prog, Vars <: Record, O](implicit prog: Eval[prog, Vars, O]): Aux[do_[prog], Vars, Vars] =
    interpret(vars => {prog.value.run(vars); vars})

  implicit def constProg[value <: String, Vars <: Record](implicit valueOf: Witness.Aux[value]): Aux[const[value], Vars, String] =
    interpret(_ => valueOf.value)

  implicit def concatProg: Aux[concat, (String, String), String] =   interpret{case (x, y) => x + y}




  implicit def defineProg[pname, ptyp, expr, Vars <: Record, EVars <: Record, Out]
  (implicit update: UpdateRec.Aux[pname, Vars, ptyp, EVars],
   expr: Eval[expr, EVars, Out]): Aux[define[pname, ptyp, expr], Vars, ptyp => Out] =
    interpret(vars => param => expr.value.run(update(param, vars)))

  implicit def callProg[I, O] : Aux[call, (I => O, I), O] = interpret{case (f, x) => f(x)}


  implicit object interpreter extends Interpreter[Aux] {
    def init[x, I] = new InitPA[x, I]
    class InitPA[x, I] {
      def apply[O]()(implicit term: Aux[x, I, O]): Aux[Unit, I, O] = term.as[Unit]
    }

    def combine[y] = new CombinePA[y]
    class CombinePA[y] {
      def apply[M, I, O](prefix: Aux[Unit, I, M])(implicit term: Aux[y, M, O]): Aux[Unit, I, O] =
        new Term[Unit, I] {
          type Output = O
          def run: I => O = x => term.run(prefix.run(x))
        }
    }
  }

  object runners {
    import meetup.control.{Display, DisplayType, RNil, Record}
    import meetup.control.Record.RNil

    import scala.reflect.runtime.universe._

    def runProg[T]()(implicit prog: EvalU[T, RNil]): Unit = runProgI(RNil)
    def runProgI[T, I](x: I)(implicit prog: EvalU[T, I]): Unit = prog.value.run(x)
    def withTypeTag[T](implicit prog: EvalU[T, RNil]) = new RunProgTPA[T, prog.Output](prog)

    class RunProgTPA[T, O](prog: Eval[T, RNil, O]) {
      def run()(implicit tt: TypeTag[O]): Unit = {
        prog.value.run(RNil)
        println(tt)
      }
    }

    def withDisplayOut[T](implicit prog: EvalU[T, RNil]) = new DisplayOuyPA[T, prog.Output](prog)

    class DisplayOuyPA[T, O](prog: Eval[T, RNil, O]) {
      def run[D]()(implicit dd: Display[O]): Unit = {

        dd(prog.value.run(RNil)).iterator.map { case (v, (kt, vt)) => s"$kt -> $v : $vt" }.foreach(println)
      }
    }

  }

}
