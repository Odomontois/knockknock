package meetup.control

import cats.free.Free
import RecordF.field
import cats.~>

import scala.annotation.showAsInfix


sealed trait RecordF[F[_]]

case class RNilF[F[_]]() extends RecordF[F]

sealed trait RecordValueF[F[_]]


final case class ValueF[F[_], A](value: Free[F, A]) extends RecordValueF[F]
final case class FuncF[F[_], A, B <: RecordValueF[F]](f: A => B) extends RecordValueF[F]


@showAsInfix
final case class RConsF[F[_], K, V <: RecordValueF[F], T <: RecordF[F]](head: V, tail: T) extends RecordF[F]

object RecordF {


  def value[F[_]] = new ValuePA[F]
  class ValuePA[F[_]] {
    def apply[A](value: F[A]): ValueF[F, A] = ValueF[F, A](Free.liftF(value))
    def pure[A](value: A): ValueF[F, A] = ValueF[F, A](Free.pure(value))
  }

  final case class field[F[_], K, V <: RecordValueF[F]](value: V) extends AnyVal

  implicit class RecOps[F[_], R <: RecordF[F]](val self: R) extends AnyVal {
    def ::[K, V <: RecordValueF[F]](fld: field[F, K, V]): RConsF[F, K, V, R] = RConsF[F, K, V, R](fld.value, self)
    def +[K, V <: RecordValueF[F]](fld: field[F, K, V])(implicit update: UpdateRecF[F, K, R, V]): update.Out = update(fld.value, self)
    def get[K](implicit select: SelectRecF[F, K, R]): select.Out = select(self)
  }
}

trait UpdateRecF[F[_], K, R <: RecordF[F], i <: RecordValueF[F]] {
  type Out <: RecordF[F]
  def apply(x: i, rec: R): Out
}

trait UpdateRecLowPriorF {

  import UpdateRecF.Aux

  implicit def updateNext[F[_], K, K1, A <: RecordValueF[F], R <: RecordF[F], x <: RecordValueF[F]]
  (implicit next: UpdateRecF[F, K, R, x]): Aux[F, K, RConsF[F, K1, A, R], x, RConsF[F, K1, A, next.Out]] =
    new UpdateRecF[F, K, RConsF[F, K1, A, R], x] {
      type Out = RConsF[F, K1, A, next.Out]
      def apply(x: x, rec: RConsF[F, K1, A, R]): Out = RConsF(rec.head, next(x, rec.tail))
    }
}

object UpdateRecF extends UpdateRecLowPrior {
  type Aux[F[_], K, R <: RecordF[F], i <: RecordValueF[F], O <: RecordF[F]] = UpdateRecF[F, K, R, i] {type Out = O}
  def apply[F[_], K, R <: RecordF[F], i <: RecordValueF[F]]
  (implicit update: UpdateRecF[F, K, R, i]): Aux[F, K, R, i, update.Out] = update


  implicit def updateNil[F[_], K, x <: RecordValueF[F]]: Aux[F, K, RNilF[F], x, RConsF[F, K, x, RNilF[F]]] = new UpdateRecF[F, K, RNilF[F], x] {
    type Out = RConsF[F, K, x, RNilF[F]]
    def apply(x: x, rec: RNilF[F]) = RConsF[F, K, x, RNilF[F]](x, RNilF())
  }

  implicit def updateHead[F[_], K, A <: RecordValueF[F], x <: RecordValueF[F], R <: RecordF[F]]:
  Aux[F, K, RConsF[F, K, A, R], x, RConsF[F, K, x, R]] =
    new UpdateRecF[F, K, RConsF[F, K, A, R], x] {
      type Out = RConsF[F, K, x, R]
      def apply(x: x, rec: RConsF[F, K, A, R]): Out = RConsF(x, rec.tail)
    }
}

trait SelectRecF[F[_], K, R <: RecordF[F]] {
  type Out <: RecordValueF[F]
  def apply(x: R): Out
}

trait LowPrioritySelectF {

  import SelectRecF.Aux

  implicit def selectNext[F[_], K, K1, A <: RecordValueF[F], R <: RecordF[F]]
  (implicit next: SelectRecF[F, K, R]): Aux[F, K, RConsF[F, K1, A, R], next.Out] = new SelectRecF[F, K, RConsF[F, K1, A, R]] {
    type Out = next.Out
    def apply(x: RConsF[F, K1, A, R]): next.Out = next(x.tail)
  }
}

object SelectRecF extends LowPrioritySelectF {
  type Aux[F[_], K, R <: RecordF[F], O <: RecordValueF[F]] = SelectRecF[F, K, R] {type Out = O}

  def apply[F[_], K, R <: RecordF[F]](implicit select: SelectRecF[F, K, R]): Aux[F, K, R, select.Out] = select

  implicit def selectHead[F[_], K, A <: RecordValueF[F], R <: RecordF[F]]: Aux[F, K, RConsF[F, K, A, R], A] =
    new SelectRecF[F, K, RConsF[F, K, A, R]] {
      type Out = A
      def apply(x: RConsF[F, K, A, R]): A = x.head
    }
}

trait LiftValueF[F[_], G[_], V <: RecordValueF[F]] {
  type Out <: RecordValueF[G]

  def apply(r: V, f: F ~> G): Out
}

object LiftValueF {
  type Aux[F[_], G[_], V <: RecordValueF[F], O <: RecordValueF[G]] = LiftValueF[F, G, V] {type Out = O}

  def apply[F[_], G[_], R <: RecordValueF[F]](implicit lift: LiftValueF[F, G, R]): Aux[F, G, R, lift.Out] = lift

  implicit def liftVal[F[_], G[_], A]: Aux[F, G, ValueF[F, A], ValueF[G, A]] =
    new LiftValueF[F, G, ValueF[F, A]] {
      override type Out = ValueF[G, A]
      override def apply(r: ValueF[F, A], f: F ~> G): Out = ValueF(r.value.mapK(f))
    }

  implicit def liftFunc[F[_], G[_], A, B <: RecordValueF[F]]
  (implicit liftOut: LiftValueF[F, G, B]): Aux[F, G, FuncF[F, A, B], FuncF[G, A, liftOut.Out]] =
    new LiftValueF[F, G, FuncF[F, A, B]] {
      override type Out = FuncF[G, A, liftOut.Out]
      override def apply(r: FuncF[F, A, B], f: F ~> G): Out = FuncF(x => liftOut(r.f(x), f))
    }
}

trait LiftRecF[F[_], G[_], R <: RecordF[F]] {
  type Out <: RecordF[G]

  def apply(r: R, f: F ~> G): Out
}

object LiftRecF{
  type Aux[F[_], G[_], R <: RecordF[F], O <: RecordF[G]] = LiftRecF[F, G, R] {type Out = O}

  def apply[F[_], G[_], R <: RecordF[F]](implicit lift: LiftRecF[F, G, R]): Aux[F, G, R, lift.Out] = lift

  implicit def liftNil[F[_], G[_]]: Aux[F, G, RNilF[F], RNilF[G]] =
    new LiftRecF[F, G, RNilF[F]] {
      type Out = RNilF[G]
      override def apply(r: RNilF[F], f: F ~> G): RNilF[G] = RNilF()
    }

  implicit def liftCons[F[_], G[_], K, V <: RecordValueF[F], R <: RecordF[F]]
  (implicit liftHead: LiftValueF[F, G, V], liftTail: LiftRecF[F, G, R]):
  Aux[F, G, RConsF[F, K, V, R], RConsF[G, K, liftHead.Out, liftTail.Out]] =
    new LiftRecF[F, G, RConsF[F, K, V, R]] {
      type Out = RConsF[G, K, liftHead.Out, liftTail.Out]
      override def apply(r: RConsF[F, K, V, R], f: F ~> G): RConsF[G, K, liftHead.Out, liftTail.Out] =
        RConsF(liftHead(r.head, f), liftTail(r.tail, f))
    }
}

import scala.reflect.runtime.universe._

trait DisplayF[F[_], R <: RecordF[F]] {
  def apply(r: R): List[(String, (Type, Type))]
}

object DisplayF {

  implicit def nilDisplayF[F[_]]: DisplayF[F, RNilF[F]] = _ => List.empty

  implicit def consDisplay[F[_], K, V <: RecordValueF[F], R <: RecordF[F]]
  (implicit next: DisplayF[F, R], kt: TypeTag[K], vt: TypeTag[V]): DisplayF[F, RConsF[F, K, V, R]] =
    r => (r.head.toString -> (kt.tpe -> vt.tpe)) :: next(r.tail)

}



