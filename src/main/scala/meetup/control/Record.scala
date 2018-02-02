package meetup.control

import Record._
import meetup.control.UpdateRec1.Aux


sealed trait Record

object RNil extends Record
final case class RCons[K, V, T <: Record](head: V, tail: T) extends Record

object Record {
  type RNil = RNil.type


  final class field[K, V] (val value: V) extends AnyVal
  def field[K] = new MkField[K]
  class MkField[K]{
    def apply[V](value: V): field[K, V] = new field(value)
  }

  implicit class RecOps[R <: Record](val self: R) extends AnyVal{
    def ::[K, V](fld: field[K, V]): RCons[K, V, R] = RCons(fld.value, self)
    def +[K, V](fld: field[K, V])(implicit update: UpdateRec1[K, R]): update.Out[V] = update(fld.value, self)
    def get[K](implicit select: SelectRec[K, R]): select.Out = select(self)
  }
}

trait UpdateRec1[K, R <: Record]{
  type Out[x] <: Record
  def apply[x](x: x, rec: R): Out[x]
}


trait UpdateLowPriority{
  implicit def updateNext[K, K1, A, R <: Record, O[x] <: Record]
  (implicit next: UpdateRec1.Aux[K, R, O]): Aux[K, RCons[K1, A, R], λ[x => RCons[K1, A, O[x]]]] =
  new UpdateRec1[K, RCons[K1, A, R]]{
    type Out[x] = RCons[K1, A, O[x]]
    def apply[x](x: x, rec: RCons[K1, A, R]): Out[x] = field[K1](rec.head) :: next(x, rec.tail)
  }
}
object UpdateRec1  {
  type Aux[K, R <: Record, O[x] <: Record] = UpdateRec1[K, R]{type Out[x] = O[x]}

  implicit def updateNil[K]: Aux[K, RNil, λ[x => RCons[K, x, RNil]]] = new UpdateRec1[K, RNil]{
    type Out[x] = RCons[K, x, RNil]
    def apply[x](x: x, rec: RNil) = RCons(x, RNil)
  }

  implicit def updateHead[K, A, R <: Record]: Aux[K, RCons[K, A, R], λ[x => RCons[K, x, R]]] = new UpdateRec1[K, RCons[K, A, R]]{
    type Out[x] = RCons[K, x, R]
    def apply[x](x: x, rec: RCons[K, A, R]): Out[x] = field[K](x) :: rec.tail
  }


}

trait UpdateRec[K, R <: Record, i]{
  type Out <: Record
  def apply(x: i, rec: R): Out
}

trait UpdateRecLowPrior{
  import UpdateRec.Aux
  implicit def updateNext[K, K1, A, R <: Record, x, O <: Record]
  (implicit next: Aux[K, R, x, O]): Aux[K, RCons[K1, A, R], x, RCons[K1, A, O]] =
    new UpdateRec[K, RCons[K1, A, R], x]{
      type Out = RCons[K1, A, O]
      def apply(x: x, rec: RCons[K1, A, R]): Out = field[K1](rec.head) :: next(x, rec.tail)
    }
}

object UpdateRec extends UpdateRecLowPrior {
  type Aux[K, R <: Record, i, O <: Record] = UpdateRec[K, R, i] {type Out = O}
  implicit def updateNil[K, x]: Aux[K, RNil, x, RCons[K, x, RNil]] = new UpdateRec[K, RNil, x]{
    type Out = RCons[K, x, RNil]
    def apply(x: x, rec: RNil) = RCons(x, RNil)
  }

  implicit def updateHead[K, A, x, R <: Record]: Aux[K, RCons[K, A, R], x, RCons[K, x, R]] = new UpdateRec[K, RCons[K, A, R], x]{
    type Out = RCons[K, x, R]
    def apply(x: x, rec: RCons[K, A, R]): Out = field[K](x) :: rec.tail
  }
}

trait SelectRec[K, R <: Record]{
  type Out
  def apply(x: R): Out
}

trait LowPrioritySelect{
  import SelectRec.Aux
  implicit def selectNext[K, K1, A, R <: Record, O]
  (implicit  next: SelectRec.Aux[K, R, O]): Aux[K, RCons[K1, A, R], O] = new SelectRec[K, RCons[K1, A, R]]{
    type Out = O
    def apply(x: RCons[K1, A, R]): O = next(x.tail)
  }
}

object SelectRec extends LowPrioritySelect {
  type Aux[K, R <: Record, O] = SelectRec[K, R]{type Out = O}

  implicit def selectHead[K, A, R <: Record]: Aux[K, RCons[K, A, R], A] = new SelectRec[K, RCons[K, A, R]]{
    type Out = A
    def apply(x: RCons[K, A, R]): A = x.head
  }
}

trait ToList[R <: Record]{
  def apply(r: R) : List[(Any, Any)]
}

object ToList{
  implicit def nilToList: ToList[RNil] = new ToList[RNil]{
    override def apply(r: RNil): List[(Any, Any)] = List.empty
  }

  implicit def consToList[ K, V, R <: Record ](implicit value: ValueOf[K], next: ToList[R]): ToList[RCons[K, V, R]] = new ToList[RCons[K, V, R]]{
    override def apply(r: RCons[K, V, R]): List[(Any, Any)] = (value.value -> r.head) :: next(r.tail)
  }
}

final case class ValueOf[T](value: T)



