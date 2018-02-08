package meetup.graphs

import meetup.control._
import meetup.graphs.console._
import Record._
import meetup.graphs.lib.concat

import scala.io.StdIn

trait Interpret[x] {
  type Input
  type Output
  def run: Input => Output

  type Self = Interpret.Aux[x, Input, Output]
}

object Interpret {
  type Aux[x, I, O] = Interpret[x] {
    type Input = I
    type Output = O
  }

  type AuxI[x, I] = Interpret[x] {
    type Input = I
  }


  case class interpret[x, I, O](run: I => O) extends Interpret[x] {
    type Input = I
    type Output = O
  }

  implicit def readLineProg[name <: String, Vars <: Record](implicit name: ValueOf[name]): Aux[readLine[name], Vars, String] =
    interpret(_ => StdIn.readLine(s"input ${name.value}:\n"))

  implicit def putLineProg[name <: String](implicit name: ValueOf[name]): Aux[putLine[name], String, Unit] =
    interpret(value => println(s"[${name.value}] $value"))


  implicit def execProg[prog, Out](implicit prog: Aux[prog, RNil, Out]): Aux[exec[prog], Unit, Unit] =
    interpret(_ => {prog.run(RNil); ()})


  implicit def consProg[x, y, A, B, C](implicit x: Aux[x, A, B], y: Aux[y, B, C]): Aux[x >> y, A, C] =
    interpret(input => y.run(x.run(input)))







  implicit def varOutputProg[x, Vars <: Record, name, Out, ROut <: Record]
  (implicit x: Aux[x, Vars, Out],
   update: UpdateRec.Aux[name, Vars, Out, ROut]): Aux[x ->> name, Vars, ROut] =
    interpret(vars => update(x.run(vars), vars))


  implicit def varInputProg[x, name, In, Out, Vars <: Record]
  (implicit select: SelectRec.Aux[name, Vars, In],
   x: Aux[x, In, Out]): Aux[x <<- name, Vars, Out] =
    interpret(vars => x.run(select.apply(vars)))


  implicit def varInput2Prog[x, name1, name2, In1, In2, Out, Vars <: Record]
  (implicit select1: SelectRec.Aux[name1, Vars, In1],
   select2: SelectRec.Aux[name2, Vars, In2],
   x: Aux[x, (In1, In2), Out]): Aux[x <<- (name1, name2), Vars, Out] =
    interpret(vars => x.run((select1(vars), select2(vars))))


  implicit def comboProg[x, y, Vars <: Record, Out1, Out2]
    (implicit x: Aux[x, Vars, Out1], y: Aux[y, Vars, Out2]): Aux[(x, y), Vars, (Out1, Out2)] =
    interpret(vars => (x.run(vars), y.run(vars)))

  implicit def doProg[prog, Vars <: Record](implicit prog: AuxI[prog, Vars]): Aux[do_[prog], Vars, Vars] =
    interpret(vars => {prog.run(vars); vars})


  implicit def constProg[value <: String, Vars <: Record](implicit valueOf: ValueOf[value]): Aux[const[value], Vars, String] =
    interpret(_ => valueOf.value)

  implicit def concatProg: Aux[concat, (String, String), String] =   interpret((x, y) => x + y)

  implicit def defineProg[pname, ptyp, expr, Vars <: Record, EVars <: Record, Out]
  (implicit update: UpdateRec.Aux[pname, Vars, ptyp, EVars],
   expr: Aux[expr, EVars, Out]): Aux[define[pname, ptyp, expr], Vars, ptyp => Out] =
    interpret(vars => param => expr.run(update(param, vars)))


  implicit def callProg[I, O] : Aux[call, (I => O, I), O] = interpret((f, x) => f(x))

  implicit def getProg[name, Vars <: Record, Out](implicit select: SelectRec.Aux[name, Vars, Out]): Aux[get[name], Vars, Out] =
    interpret(vars => select(vars))


  def runProg[T, O]()(implicit prog: Aux[T, Unit, O]): Unit = runProgI(())
  def runProgI[T, I, O](x: I)(implicit prog: Aux[T, I, O]): Unit = prog.run(x)
}

