package meetup.control

import cats._

import scala.reflect.runtime.universe._
import RecordF.value
import cats.arrow.FunctionK
import cats.data.{Reader, State, StateT}

object RecFs extends App{
  type Lol = "Lol"
  type Kek = "Kek"
  type xxx[F[_]] = RConsF[F, Lol, ValueF[F, Int], RConsF[F, Kek, ValueF[F, String], RNilF[F]]]
  val xxx : xxx[Eval] = RConsF(  value(Eval.now(1)), RConsF(value(Eval.now("lol")), RNilF()))


  implicitly[SelectRecF[Eval, Lol, xxx[Eval]]]
  val x = SelectRecF[Eval, Kek, xxx[Eval]].apply(xxx)
  println(x.value.runTailRec.value)
  println(weakTypeOf[x.type].widen.dealias)

  def vvv[A](x: Eval[A]): Id[A] = x.value
  def www[A](x: Eval[A]): State[String, A] = StateT.liftF(x)
  val yyy = LiftRecF[Eval, Id, xxx[Eval]].apply(xxx, FunctionK.lift[Eval, Id](vvv))
  val zzz = LiftRecF[Eval, State[String, ?], xxx[Eval]].apply(xxx, FunctionK.lift(www))
  println(typeOf[yyy.type].widen)
  println(typeOf[zzz.type].widen)
}
