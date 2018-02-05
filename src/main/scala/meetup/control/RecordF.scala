package meetup.control

import Record._
import cats.free.Free

import scala.annotation.showAsInfix


sealed trait RecordF[F[_]]

case class RNilF[F[_]]() extends RecordF[F]

sealed trait RecordValueF[F[_]]

final case class LiftedValueF[F[_], A](value: F[A]) extends RecordValueF[F]
final case class LiftedFuncF[F[_], A, B <: RecordValueF[F]](f: A => B) extends RecordValueF[F]


@showAsInfix
final case class RConsF[K, F[_], V <: RecordValueF[F], T <: RecordF[F]](head: V, tail: T) extends RecordF[F]

object RecordF {
  def value[F[_]] = new ValuePA[F]
  class ValuePA[F[_]]{
    def apply[A](value: F[A]): LiftedValueF[F, A] = new LiftedValueF[F, A](value)
  }

  final case class field[F[_], K, V <: RecordValueF[F]](value: V) extends AnyVal

  implicit class RecOps[F[_], R <: RecordF[F]](val self: R) extends AnyVal {
    def ::[K, V](fld: field[F, K, V]): RConsF[K, F, V, R] = RConsF[K, F, V, R](fld.value, self)
    def +[K, V](fld: field[F, K, V])(implicit update: UpdateRecF[F, K, R, V]): update.Out = update(fld.value, self)
    def get[K](implicit select: SelectRec[K, R]): select.Out = select(self)
  }
}

trait UpdateRecF[F[_], K, R <: Record, i] {
  type Out <: RecordF[F]
  def apply(x: i, rec: R): Out
}

trait UpdateRecLowPriorF {

  import UpdateRec.Aux

  implicit def updateNext[F[_], K, K1, A, R <: Record, x, O <: Record]
  (implicit next: Aux[K, R, x, O]): Aux[K, RCons[K1, A, R], x, RCons[K1, A, O]] =
    new UpdateRec[K, RCons[K1, A, R], x] {
      type Out = RCons[K1, A, O]
      def apply(x: x, rec: RCons[K1, A, R]): Out = field[K1](rec.head) :: next(x, rec.tail)
    }
}

object UpdateRecF extends UpdateRecLowPrior {
  type Aux[K, R <: Record, i, O <: Record] = UpdateRec[K, R, i] {type Out = O}
  implicit def updateNil[K, x]: Aux[K, RNil, x, RCons[K, x, RNil]] = new UpdateRec[K, RNil, x] {
    type Out = RCons[K, x, RNil]
    def apply(x: x, rec: RNil) = RCons(x, RNil)
  }

  implicit def updateHead[K, A, x, R <: Record]: Aux[K, RCons[K, A, R], x, RCons[K, x, R]] =
    new UpdateRec[K, RCons[K, A, R], x] {
      type Out = RCons[K, x, R]
      def apply(x: x, rec: RCons[K, A, R]): Out = field[K](x) :: rec.tail
    }
}

trait SelectRecF[K, R <: Record] {
  type Out
  def apply(x: R): Out
}

trait LowPrioritySelectF {

  import SelectRec.Aux

  implicit def selectNext[K, K1, A, R <: Record, O]
  (implicit next: SelectRec.Aux[K, R, O]): Aux[K, RCons[K1, A, R], O] = new SelectRec[K, RCons[K1, A, R]] {
    type Out = O
    def apply(x: RCons[K1, A, R]): O = next(x.tail)
  }
}

object SelectRecF extends LowPrioritySelect {
  type Aux[K, R <: Record, O] = SelectRec[K, R] {type Out = O}

  implicit def selectHead[K, A, R <: Record]: Aux[K, RCons[K, A, R], A] = new SelectRec[K, RCons[K, A, R]] {
    type Out = A
    def apply(x: RCons[K, A, R]): A = x.head
  }
}


import scala.reflect.runtime.universe._

trait DisplayF[R] {
  def apply(r: R): List[(String, (Type, Type))]
}

object DisplayF {

  implicit def nilDisplay: Display[RNil] = r => List.empty

  implicit def consDisplay[K, V, R <: Record](implicit next: Display[R], kt: TypeTag[K], vt: TypeTag[V]): Display[RCons[K, V, R]] = r => {
    (r.head.toString -> (kt.tpe -> vt.tpe)) :: next(r.tail)
  }
}


final case class ValueOf[T](value: T)



