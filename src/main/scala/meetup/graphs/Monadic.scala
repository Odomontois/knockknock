package meetup.graphs
import cats.arrow.FunctionK
import cats.data.{ReaderT, WriterT}
import cats.mtl.{ApplicativeAsk, ApplicativeLayer, FunctorLayer, FunctorTell}
import cats.{Applicative, Apply, FlatMap, Functor, Id, Monad, Monoid, ~>}
import meetup.control.Record
import meetup.graphs.Term.Aux
import shapeless.Witness
import meetup.control._
import meetup.graphs.console._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.instances.vector._

trait Monadic[x, I] {
  type Output

  def run: I => Output
}

object Monadic {
  type Aux[x, I, O] = Monadic[x, I] {type Output = O}
  type Eval[x, I, O] = Interpret.Aux[Aux, x, I, O]
  type EvalU[x, I] = Interpret[Aux, x, I]
  case class Val[F[_], A](x: F[A])

  case class interpret[x, I, O](run: I => O) extends Monadic[x, I] {
    type Output = O
  }

  case class mapVal[x, F[_]: Functor, I, O](f: I => O) extends Monadic[x, Val[F, I]] {
    type Output = Val[F, O]
    val run = (v: Val[F, I]) => Val(v.x.map(f))
  }

  type UserInput = Map[String, String]

  implicit def readLineProg[name <: String, F[_], Vars <: Record, G[_]]
    (implicit name: Witness.Aux[name], ensure: EnsureReader.Aux[F, UserInput, G], G: Apply[G]): Aux[readLine[name], Val[F, Vars], Val[G,String]] =
    interpret(fi => Val(ensure.trans(fi.x) *> ensure.reader.ask.map(_(name.value))))


  type UserOutput = Vector[String]
  implicit def putLineProg[name <: String, F[_], G[_]]
    (implicit name: Witness.Aux[name], ensure: EnsureWriter.Aux[F, UserOutput, G], G: FlatMap[G]): Aux[putLine[name], Val[F, String], Val[G, Unit]] =
    interpret(fi => Val( ensure.trans(fi.x) >>= (s => ensure.writer.tell(Vector(s)))))


  object runners {
    import meetup.control.{Display, DisplayType, RNil, Record}
    import meetup.control.Record.RNil

    import scala.reflect.runtime.universe._

    type Base = Val[Id, RNil]
    val Base: Base = Val[Id, RNil](RNil)

    def runProg[T]()(implicit prog: EvalU[T, Base]): Unit = runProgI(Base)
    def runProgI[T, I](x: I)(implicit prog: EvalU[T, I]): Unit = prog.run(x)
    def withTypeTag[T](implicit prog: EvalU[T, Val[Id, RNil]]) = new RunProgTPA[T, prog.Output](prog)

    class RunProgTPA[T, O](prog: Eval[T, Val[Id, RNil], O]) {
      def run(implicit tt: TypeTag[O]): O = {
        println(tt)
        prog.run(Base)
      }
    }

    def withDisplayOut[T](implicit prog: EvalU[T, RNil]) = new DisplayOuyPA[T, prog.Output](prog)

    class DisplayOuyPA[T, O](prog: Eval[T, RNil, O]) {
      def run[D]()(implicit dd: Display[O]): Unit = {

        dd(prog.run(RNil)).iterator.map { case (v, (kt, vt)) => s"$kt -> $v : $vt" }.foreach(println)
      }
    }
  }

  implicit val interpreter: Interpreter[Aux] = new Interpreter[Aux] {
    def initial[x, I, O](implicit term: Aux[x, I, O]): Interpret.Aux[Aux, Unit, I, O] = new Interpret[Aux, Unit, I] {
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

import cats.mtl.instances.readert._
import cats.mtl.instances.writert._
import cats.mtl.instances.local._
import cats.mtl.instances.listen._
import cats.mtl.hierarchy.base._

trait EnsureReader[F[_], R] {
  type Result[_]
  def reader: ApplicativeAsk[Result, R]
  def trans: F ~> Result
}


object EnsureReader extends FallbackEnsureReader {
  type Aux[F[_], R, G[_]] = EnsureReader[F, R] {type Result[x] = G[x]}

  implicit def useExisting[F[_], R](implicit ask: ApplicativeAsk[F, R]): Aux[F, R, F] =
    new EnsureReader[F, R] {
      type Result[x] = F[x]
      def reader = ask
      def trans = FunctionK.id
    }
}


trait FallbackEnsureReader {
  final implicit def upgrade[F[_], R](implicit F: Applicative[F]): EnsureReader.Aux[F, R, ReaderT[F, R, ?]] =
    new EnsureReader[F, R] {
      type Result[x] = ReaderT[F, R, x]
      val reader: ApplicativeAsk[ReaderT[F, R, ?], R] = ApplicativeAsk[Result, R]
      val trans: ~>[F, ReaderT[F, R, ?]] = FunctionK.lift((x: F[Any]) => ApplicativeLayer[Result, F].layer(x))
    }
}

trait EnsureWriter[F[_], W] {
  type Result[_]
  def writer: FunctorTell[Result, W]
  def trans: F ~> Result
}

object EnsureWriter extends FallbackEnsureWriter {
  type Aux[F[_], W, G[_]] = EnsureWriter[F, W] {type Result[x] = G[x]}

  implicit def useExisting[F[_], W](implicit tell: FunctorTell[F, W]): Aux[F, W, F] =
    new EnsureWriter[F, W] {
      type Result[x] = F[x]
      def writer = tell
      def trans = FunctionK.id
    }
}

trait FallbackEnsureWriter{
  final implicit def upgrade[F[_], W](implicit F: Monad[F], W: Monoid[W]): EnsureWriter.Aux[F, W, WriterT[F, W, ?]] =
    new EnsureWriter[F, W] {
      type Result[x] = WriterT[F, W, x]
      val writer: FunctorTell[WriterT[F, W, ?], W] = FunctorTell[Result, W]
      val trans: ~>[F, WriterT[F, W, ?]] = FunctionK.lift((x: F[Any]) => FunctorLayer[Result, F].layer(x))
    }
}

