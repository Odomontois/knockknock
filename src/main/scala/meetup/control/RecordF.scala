package meetup.control

import RecordF._

import scala.annotation.showAsInfix


sealed trait RecordF[F[_]]

case class RNilF[F[_]]() extends RecordF[F]

sealed trait RecordValueF[F[_]]

final case class LiftedValueF[F[_], A](value: F[A]) extends RecordValueF[F]
final case class LiftedFuncF[F[_], A, B <: RecordValueF[F]](f: A => B) extends RecordValueF[F]


@showAsInfix
final case class RConsF[F[_], K, V <: RecordValueF[F], T <: RecordF[F]](head: V, tail: T) extends RecordF[F]

object RecordF {
  def value[F[_]] = new ValuePA[F]
  class ValuePA[F[_]] {
    def apply[A](value: F[A]): LiftedValueF[F, A] = new LiftedValueF[F, A](value)
  }

  final case class field[F[_], K, V <: RecordValueF[F]](value: V) extends AnyVal

  implicit class RecOps[F[_], R <: RecordF[F]](val self: R) extends AnyVal {
    def ::[K, V <: RecordValueF[F]](fld: field[F, K, V]): RConsF[F, K, V, R] = RConsF[F, K, V, R](fld.value, self)
    def +[K, V <: RecordValueF[F]](fld: field[F, K, V])(implicit update: UpdateRecF[F, K, R, V]): update.Out = update(fld.value, self)
    def get[K](implicit select: SelectRecF[F, K, R]): select.Out = select(self)
  }
}

trait UpdateRecF[F[_], K, R <: Record, V <: RecordValueF[F]] {
  type Out <: RecordF[F]
  def apply(x: V, rec: R): Out
}

trait UpdateRecLowPriorF {

  import UpdateRecF.Aux

  final implicit def updateNext[F[_], K, K1, A <: RecordValueF[F], R <: Record, V <: RecordValueF[F]]
  (implicit next: UpdateRecF[F, K, R, V]): Aux[F, K, RConsF[F, K1, A, R], V, RConsF[F, K1, A, next.Out]] =
    new UpdateRecF[F, K, RConsF[F, K1, A, R], V] {
      type Out = RConsF[F, K1, A, next.Out]
      def apply(x: V, rec: RConsF[F, K1, A, R]): Out = field[F, K1, A](rec.head) :: next(x, rec.tail)
    }
}

object UpdateRecF extends UpdateRecLowPriorF {
  type Aux[F[_], K, R <: Record, i <: RecordValueF[F], O <: RecordF[F]] = UpdateRecF[F, K, R, i] {type Out = O}
  implicit def updateNil[F, K, x]: Aux[F, K, RNilF[F], x, RConsF[F, K, x, RNilF[F]]] = new UpdateRecF[F, K, RNilF[F], x] {
    type Out = RConsF[F, K, x, RNilF[F]]
    def apply(x: x, rec: RNilF[F]) = RConsF(x, RNilF())
  }

  implicit def updateHead[F[_], K, A <: RecordValueF[F], x <: RecordValueF[F], R <: Record]: Aux[F, K, RCons[K, A, R], x, RCons[K, x, R]] =
    new UpdateRecF[F, K, RCons[K, A, R], x] {
      type Out = RConsF[F,K, x, R]
      def apply(x: x, rec: RConsF[F, K, A, R]): Out = field[F, K , A](x) :: rec.tail
    }
}

trait SelectRecF[F[_], K, R <: Record] {
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



