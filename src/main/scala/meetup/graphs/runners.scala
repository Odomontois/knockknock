package meetup.graphs
import meetup.control.{Display, DisplayType, RNil, Record}
import meetup.control.Record.RNil

import scala.reflect.runtime.universe._

object runners {

  def runProg[T]()(implicit prog: Interpret[T, RNil]): Unit = runProgI(RNil)
  def runProgI[T, I](x: I)(implicit prog: Interpret[T, I]): Unit = prog.run(x)
  def withTypeTag[T](implicit prog: Interpret[T, RNil]) = new RunProgTPA[T, prog.Output](prog)

  class RunProgTPA[T, O](prog: Interpret.Aux[T, RNil, O]) {
    def run()(implicit tt: TypeTag[O]): Unit = {
      prog.run(RNil)
      println(tt)
    }
  }

  def withDisplayOut[T](implicit prog: Interpret[T, RNil]) = new DisplayOuyPA[T, prog.Output](prog)

  class DisplayOuyPA[T, O](prog: Interpret.Aux[T, RNil, O]) {
    def run[D]()(implicit dd: Display[O]): Unit = {

      dd(prog.run(RNil)).iterator.map { case (v, (kt, vt)) => s"$kt -> $v : $vt" }.foreach(println)
    }
  }

}
