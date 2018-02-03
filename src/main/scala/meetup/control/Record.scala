package meetup.control

import Record._

import scala.annotation.showAsInfix


sealed trait Record

object RNil extends Record

@showAsInfix
final case class RCons[K, V, T <: Record](head: V, tail: T) extends Record

object Record {

  type RNil = RNil.type


  @showAsInfix
  final class #->[K, V](val value: V) extends AnyVal

  @showAsInfix
  final case class %::[F, V](head: F, tail: V)

  def field[K] = new MkField[K]
  class MkField[K] {
    def apply[V](value: V): #->[K, V] = new #->(value)
  }

  implicit class RecOps[R <: Record](val self: R) extends AnyVal {
    def ::[K, V](fld: #->[K, V]): RCons[K, V, R] = RCons(fld.value, self)
    def +[K, V](fld: #->[K, V])(implicit update: UpdateRec[K, R, V]): update.Out = update(fld.value, self)
    def get[K](implicit select: SelectRec[K, R]): select.Out = select(self)
  }
}

trait UpdateRec[K, R <: Record, i] {
  type Out <: Record
  def apply(x: i, rec: R): Out
}

trait UpdateRecLowPrior {
  import UpdateRec.Aux
  implicit def updateNext[K, K1, A, R <: Record, x, O <: Record]
  (implicit next: Aux[K, R, x, O]): Aux[K, RCons[K1, A, R], x, RCons[K1, A, O]] =
    new UpdateRec[K, RCons[K1, A, R], x] {
      type Out = RCons[K1, A, O]
      def apply(x: x, rec: RCons[K1, A, R]): Out = field[K1](rec.head) :: next(x, rec.tail)
    }
}

object UpdateRec extends UpdateRecLowPrior {
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

trait SelectRec[K, R <: Record] {
  type Out
  def apply(x: R): Out
}

trait LowPrioritySelect {
  import SelectRec.Aux
  implicit def selectNext[K, K1, A, R <: Record, O]
  (implicit next: SelectRec.Aux[K, R, O]): Aux[K, RCons[K1, A, R], O] = new SelectRec[K, RCons[K1, A, R]] {
    type Out = O
    def apply(x: RCons[K1, A, R]): O = next(x.tail)
  }
}

object SelectRec extends LowPrioritySelect {
  type Aux[K, R <: Record, O] = SelectRec[K, R] {type Out = O}

  implicit def selectHead[K, A, R <: Record]: Aux[K, RCons[K, A, R], A] = new SelectRec[K, RCons[K, A, R]] {
    type Out = A
    def apply(x: RCons[K, A, R]): A = x.head
  }
}

trait ToList[R <: Record] {
  def apply(r: R): List[(Any, Any)]
}

object ToList {
  implicit def nilToList: ToList[RNil] = (r: RNil) => List.empty

  implicit def consToList[K, V, R <: Record](implicit value: ValueOf[K], next: ToList[R]): ToList[RCons[K, V, R]] = r => (value.value -> r.head) :: next(r.tail)
}
import scala.reflect.runtime.universe._

trait DisplayType[R] {
  type Out
  def tag: TypeTag[Out]
}

object DisplayType {
  type Aux[R, O] = DisplayType[R] {type Out = O}

  implicit def nilDisplay: Aux[RNil, RNil] = new DisplayType[RNil] {
    type Out = RNil
    override def tag: TypeTag[RNil] = typeTag[RNil]
  }

  implicit def consDisplay[K, V, R <: Record, O](implicit next: Aux[R, O], kt: TypeTag[K], vt: TypeTag[V]): Aux[RCons[K, V, R], (K #-> V) %:: O] = new DisplayType[RCons[K, V, R]] {
    type Out = (K #-> V) %:: O
    override def tag: TypeTag[Out] = {
      implicit def outTag: TypeTag[O] = next.tag
      typeTag[(K #-> V) %:: O]
    }
  }
}

trait Display[R] {
  def apply(r: R): List[(String, (Type, Type))]
}

object Display {

  implicit def nilDisplay: Display[RNil] = r => List.empty

  implicit def consDisplay[K, V, R <: Record](implicit next: Display[R], kt: TypeTag[K], vt: TypeTag[V]): Display[RCons[K, V, R]] = r => {
    (r.head.toString -> (kt.tpe -> vt.tpe)) :: next(r.tail)
  }
}


final case class ValueOf[T](value: T)



