package meetup.graphs
import cats.arrow.FunctionK
import cats.data.{ReaderT, WriterT}
import cats.mtl.{ApplicativeAsk, ApplicativeLayer, FunctorLayer, FunctorTell}
import cats.{Applicative, Apply, FlatMap, Functor, Id, Monad, Monoid, ~>}
import meetup.control.Record
import shapeless.Witness
import meetup.control._
import meetup.graphs.console._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.instances.vector._

trait Monadic[x, F[_], I] {
  type FOut[_]
  type VOut

  def int: Monadic.interpret[x, F, I, FOut, VOut]
  def run(fi: F[I]): FOut[VOut] = trans(fi).flatMap(exec)
  def trans: F ~> FOut
  def exec: I => FOut[VOut]
  implicit def monad: Monad[FOut]
  def as[y]: Monadic.Aux[y, F, I, FOut, VOut] = this.asInstanceOf[Monadic.Aux[y, F, I, FOut, VOut]]
}

object Monadic {
  type Aux[x, F[_], I, G[_], O] = Monadic[x, F, I] {type VOut = O; type FOut[a] = G[a]}
  type Eval[x, F[_], I, G[_], O] = InterpretF.Aux[Aux, x, F, I, G, O]
  type EvalU[x, F[_], I] = InterpretF[Aux, x, F, I]


  trait Ev[x, F[_], I, O] {
    type FOut[_]
    val value: Monadic.Aux[x, F, I, FOut, O]
  }

  object Ev {
    type Aux[x, F[_], I, G[_], O] = Ev[x, F, I, O] {type FOut[a] = G[a]}
    implicit def instance[x, F[_], I](implicit int: InterpretF[Monadic.Aux, x, F, I]): Aux[x, F, I, int.FOut, int.VOut] =
      new Ev[x, F, I, int.VOut] {
        type FOut[a] = int.FOut[a]
        val value = int.value
      }
  }

  case class interpret[x, F[_], A, G[_], B](trans: F ~> G, exec: A => G[B])(implicit val monad: Monad[G]) extends Monadic[x, F, A] {
    type FOut[a] = G[a]
    type VOut = B
    def int = this

    def andThen[y](implicit term: Monadic[y, G, B]): interpret[x, F, A, term.FOut, term.VOut] =
      interpret(term.trans.compose(this.trans), (x: A) => term.monad.flatMap(term.trans(this.exec(x)))(term.exec))(term.monad)
  }

  type UserInput = Map[String, String]

  implicit def readLineProg[name <: String, F[_], Vars <: Record, G[_]]
    (implicit name: Witness.Aux[name], ensure: EnsureReader[F, UserInput, G], G: Monad[G]): Aux[readLine[name], F, Vars, G, String] =
    interpret[readLine[name], F, Vars, G, String](ensure.trans, _ => ensure.reader.ask.map(_ (name.value)))


  type UserOutput = Vector[(String, String)]
  implicit def putLineProg[name <: String, F[_], G[_]]
    (implicit name: Witness.Aux[name], ensure: EnsureWriter[F, UserOutput, G], G: Monad[G]): interpret[putLine[name], F, String, G, Unit] =
    interpret(ensure.trans: F ~> G, s => ensure.writer.tell(Vector((name.value, s))))


  implicit def varOutputProg[x, F[_], Vars <: Record, name <: String, Out]
    (implicit x: Ev[x, F, Vars, Out],
     update: UpdateRec[name, Vars, Out]): interpret[x ->> name, F, Vars, x.FOut, update.Out] = {
    import x.value._
    interpret(trans, (vars: Vars) => monad.map(exec(vars))(out => update(out, vars)))(monad)
  }

  implicit def varInputProg[x, name <: String, F[_], In, Vars <: Record, Out]
    (implicit select: SelectRec.Aux[name, Vars, In],
     x: Ev[x, F, In, Out]): interpret[x <<- name, F, Vars, x.FOut, Out] = {
    import x.value._
    interpret(trans, (vars: Vars) => exec(select.apply(vars)))(monad)
  }

  implicit def doProg[prog, Vars <: Record, F[_]](implicit prog: EvalU[prog, F, Vars]):
    interpret[do_[prog], F, Vars, prog.FOut, Vars] = {
    import prog.value._
    interpret(trans, (vars: Vars) => monad.as(exec(vars), vars))(monad)
  }

  implicit def constProg[value <: String, F[_], Vars](implicit valueOf: Witness.Aux[value], monad: Monad[F]): interpret[const[value], F, Vars, F, String] =
    interpret(FunctionK.id, _ => monad.pure(valueOf.value))

//  implicit def defineProg[pname, ptyp, expr, F[_], Vars <: Record, EVars <: Record, Out]
//  (implicit update: UpdateRec.Aux[pname, Vars, ptyp, EVars],
//   ev: Ev[expr, F[_], EVars, Out]): interpret[define[pname, ptyp, expr], F[_], Vars,  ev.FOut, ptyp => Out] = {
//    import ev.value._
//    interpret(trans, vars => param => ev.value.run(update(param, vars)))
//  }


  object runners {
    import meetup.control.{Display, DisplayType, RNil, Record}
    import meetup.control.Record.RNil

    import scala.reflect.runtime.universe._

    def runProg[T]()(implicit prog: EvalU[T, Id, RNil]): Unit = runProgI[T, Id, RNil](RNil)
    def runProgI[T, F[_], I](x: F[I])(implicit prog: EvalU[T, F, I]): Unit = prog.value.run(x)
    def withTypeTag[T](implicit prog: EvalU[T, Id, RNil]) = new RunProgTPA[T, prog.FOut, prog.VOut](prog)
    class ??

    class RunProgTPA[T, G[_], O](prog: Eval[T, Id, RNil, G, O]) {
      def run(implicit tt: TypeTag[O], tf: TypeTag[G[??]]): G[O] = {
        println(s"output tag: ${tt.tpe}")
        println(s"functor tag: ${tf.tpe}")
        prog.value.run(RNil)
      }
    }

    def withDisplayOut[T](implicit prog: EvalU[T, Id, RNil]) = new DisplayOuyPA[T, prog.FOut, prog.VOut](prog)

    class DisplayOuyPA[T, G[_], O](prog: Eval[T, Id, RNil, G, O]) {
      def run[D](implicit dd: Display[O]): G[String] = {
        import prog.value.monad

        prog.value.run(RNil).map(x => dd(x).iterator.map { case (v, (kt, vt)) => s"$kt -> $v : $vt" }.mkString("\n"))
      }
    }
  }

  implicit object interpreter extends InterpreterF[Aux] {
    def init[x, F[_], A] = new InitPA[x, F, A]
    class InitPA[x, F[_], A] {
      def apply()(implicit term: Monadic[x, F, A]): interpret[Unit, F, A, term.FOut, term.VOut] = term.as[Unit].int
    }
  }
}

import cats.mtl.instances.readert._
import cats.mtl.instances.writert._
import cats.mtl.instances.local._
import cats.mtl.instances.listen._
import cats.mtl.hierarchy.base._

trait EnsureReader[F[_], R, G[_]] {
  def reader: ApplicativeAsk[G, R]
  def trans: F ~> G
}


object EnsureReader extends FallbackEnsureReader {

  implicit def useExisting[F[_], R](implicit ask: ApplicativeAsk[F, R]): EnsureReader[F, R, F] =
    new EnsureReader[F, R, F] {
      def reader = ask
      def trans = FunctionK.id
    }
}


trait FallbackEnsureReader {
  final implicit def upgrade[F[_], R](implicit F: Applicative[F]): EnsureReader[F, R, ReaderT[F, R, ?]] =
    new EnsureReader[F, R, ReaderT[F, R, ?]] {
      val reader: ApplicativeAsk[ReaderT[F, R, ?], R] = ApplicativeAsk[ReaderT[F, R, ?], R]
      val trans: ~>[F, ReaderT[F, R, ?]] = FunctionK.lift((x: F[Any]) => ApplicativeLayer[ReaderT[F, R, ?], F].layer(x))
    }
}

trait EnsureWriter[F[_], W, G[_]] {
  def writer: FunctorTell[G, W]
  def trans: F ~> G
}

object EnsureWriter extends FallbackEnsureWriter {
  implicit def useExisting[F[_], W](implicit tell: FunctorTell[F, W]): EnsureWriter[F, W, F] =
    new EnsureWriter[F, W, F] {
      def writer = tell
      def trans = FunctionK.id
    }
}

trait FallbackEnsureWriter {
  final implicit def upgrade[F[_], W](implicit F: Monad[F], W: Monoid[W]): EnsureWriter[F, W, WriterT[F, W, ?]] =
    new EnsureWriter[F, W, WriterT[F, W, ?]] {
      val writer: FunctorTell[WriterT[F, W, ?], W] = FunctorTell[WriterT[F, W, ?], W]
      val trans: ~>[F, WriterT[F, W, ?]] = FunctionK.lift((x: F[Any]) => FunctorLayer[WriterT[F, W, ?], F].layer(x))
    }
}

