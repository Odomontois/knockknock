package meetup.graphs
import meetup.control.{Display, DisplayType, RNil, Record}
import meetup.control.Record.RNil

import scala.reflect.runtime.universe._

object runners {
  import Term.{Eval, EvalU}

  def runProg[T]()(implicit prog: EvalU[T, RNil]): Unit = runProgI(RNil)
  def runProgI[T, I](x: I)(implicit prog: EvalU[T, I]): Unit = prog.run(x)
  def withTypeTag[T](implicit prog: EvalU[T, RNil]) = new RunProgTPA[T, prog.Output](prog)

  class RunProgTPA[T, O](prog: Eval[T, RNil, O]) {
    def run()(implicit tt: TypeTag[O]): Unit = {
      prog.run(RNil)
      println(tt)
    }
  }

  def withDisplayOut[T](implicit prog: EvalU[T, RNil]) = new DisplayOuyPA[T, prog.Output](prog)

  class DisplayOuyPA[T, O](prog: Eval[T, RNil, O]) {
    def run[D]()(implicit dd: Display[O]): Unit = {

      dd(prog.run(RNil)).iterator.map { case (v, (kt, vt)) => s"$kt -> $v : $vt" }.foreach(println)
    }
  }

}
