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
    interpret(vars => (x.run(vars), y.run(vars)))




  implicit def varOutputProg[x, Vars <: Record, name <: String, Out, ROut <: Record]
  (implicit x: Eval[x, Vars, Out],
   update: UpdateRec.Aux[name, Vars, Out, ROut]): Aux[x ->> name, Vars, ROut] =
    interpret(vars => update(x.run(vars), vars))

  implicit def varInputProg[x, name <: String, In, Out, Vars <: Record]
  (implicit select: SelectRec.Aux[name, Vars, In],
   x: Eval[x, In, Out]): Aux[x <<- name, Vars, Out] =
    interpret(vars => x.run(select.apply(vars)))

  implicit def varInput2Prog[x, name1 <: String, name2 <: String, In1, In2, Out, Vars <: Record]
  (implicit select1: SelectRec.Aux[name1, Vars, In1],
   select2: SelectRec.Aux[name2, Vars, In2],
   x: Eval[x, (In1, In2), Out]): Aux[x <<- (name1, name2), Vars, Out] =
    interpret(vars => x.run((select1(vars), select2(vars))))

  implicit def getProg[name, Vars <: Record, Out](implicit select: SelectRec.Aux[name, Vars, Out]): Aux[get[name], Vars, Out] =
    interpret(vars => select(vars))





  implicit def doProg[prog, Vars <: Record, O](implicit prog: Eval[prog, Vars, O]): Aux[do_[prog], Vars, Vars] =
    interpret(vars => {prog.run(vars); vars})

  implicit def constProg[value <: String, Vars <: Record](implicit valueOf: Witness.Aux[value]): Aux[const[value], Vars, String] =
    interpret(_ => valueOf.value)

  implicit def concatProg: Aux[concat, (String, String), String] =   interpret{case (x, y) => x + y}




  implicit def defineProg[pname, ptyp, expr, Vars <: Record, EVars <: Record, Out]
  (implicit update: UpdateRec.Aux[pname, Vars, ptyp, EVars],
   expr: Eval[expr, EVars, Out]): Aux[define[pname, ptyp, expr], Vars, ptyp => Out] =
    interpret(vars => param => expr.run(update(param, vars)))

  implicit def callProg[I, O] : Aux[call, (I => O, I), O] = interpret{case (f, x) => f(x)}


  def initial[x, I](implicit term: Term[x, I]): Eval[Unit, I, term.Output] = new Interpret[Aux, Unit, I] {
    type Output = term.Output
    override def run(x: I): term.Output = term.run(x)
  }

  def combine[x, I] = new CombinePA[x, I]
  class CombinePA[x, I] {
    def apply[B, O](prefix: Eval[Unit, I, B])(implicit term: Term.Aux[x, B, O]): Eval[Unit, I, O] = new Interpret[Aux, Unit, I] {
      type Output = term.Output
      override def run(x: I): term.Output = term.run(prefix.run(x))
    }
  }

  implicit val interpreter: Interpreter[Aux] = new Interpreter[Aux] {
    def initial[x, I, O](implicit term: Aux[x, I, O]): Interpret.Aux[Aux, Unit, I, O] =  new Interpret[Aux, Unit, I] {
      type Output = term.Output
      override def run(x: I): term.Output = term.run(x)
    }
    override def combine[x, I, B, O](prefix: Interpret.Aux[Aux, Unit, I, B])(implicit term: Aux[x, B, O]): Interpret.Aux[Aux, Unit, I, O] =
      new Interpret[Aux, Unit, I] {
        type Output = term.Output
        override def run(x: I): term.Output = term.run(prefix.run(x))
      }
  }

}
