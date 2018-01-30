package meetup.control

import Record._

import scala.implicits.Not

sealed trait Record

object RNil extends Record
final case class RCons[K, V, T <: Record](head: V, tail: T) extends Record

object Record {
  type RNil = RNil.type
  final case class field[K, V] (value: V) extends AnyVal

  implicit class RecOps[R <: Record](val self: R) extends AnyVal{
    def ::[K, V](fld: field[K, V]): RCons[K, V, R] = RCons(fld.value, self)
    def +[K, V](fld: field[K, V])(implicit update: UpdateRec[K, R]): update.Out[V] = update(fld.value, self)
    def get[K](implicit select: SelectRec[K, R]): select.Out = select(self)
  }
}

trait UpdateRec[K, R <: Record]{
  type Out[x] <: Record
  def apply[x](x: x, rec: R): Out[x]
}

object UpdateRec{
  type Aux[K, R <: Record, O[x] <: Record] = UpdateRec[K, R]{type Out = O}
  implicit def updateNil[K]: Aux[K, RNil, [x] => RCons[K, x, RNil]] = new UpdateRec[K, RNil]{
    type Out[x] = RCons[K, x, RNil]
    def apply[x](x: x, rec: RNil) = RCons(x, RNil)
  }

  implicit def updateHead[K, A, R <: Record]: Aux[K, RCons[K, A, R], [x] => RCons[K, x, R]] = new UpdateRec[K, RCons[K, A, R]]{
    type Out[x] = RCons[K, x, R]
    def apply[x](x: x, rec: RCons[K, A, R]): Out[x] = field[K = K](x) :: rec.tail
  }

  implicit def updateNext[K, K1, A, R <: Record]
  (implicit _e: Not[UpdateRec[K, RCons[K1, A, R]]],
   next: UpdateRec[K, R]): Aux[K, RCons[K1, A, R], [x] => RCons[K1, A, next.Out[x]]] =
    new UpdateRec[K, RCons[K1, A, R]]{
      type Out[x] = RCons[K1, A, next.Out[x]]
      def apply[x](x: x, rec: RCons[K1, A, R]): Out[x] = field[K = K1](rec.head) :: next(x, rec.tail)
    }
}

trait SelectRec[K, R <: Record]{
  type Out
  def apply(x: R): Out
}

object SelectRec{
  type Aux[K, R <: Record, O] = SelectRec[K, R]{type Out = O}


  implicit def selectHead[K, A, R <: Record]: Aux[K, RCons[K, A, R], A] = new SelectRec[K, RCons[K, A, R]]{
    type Out = A
    def apply(x: RCons[K, A, R]): A = x.head
  }

  implicit def selectNext[K, K1, A, R <: Record]
  (implicit _e: Not[SelectRec[K, R]], next: SelectRec[K, R]): Aux[K, RCons[K1, A, R], next.Out] = new SelectRec[K, RCons[K1, A, R]]{
    type Out = next.Out
    def apply(x: RCons[K, A, R]): Out = next(x.tail)
  }
}

final case class ValueOf[T](value: T)



