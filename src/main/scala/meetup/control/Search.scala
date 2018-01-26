package meetup.control

import shapeless._
import shapeless.ops.hlist.Prepend

sealed trait Search[A] {
  type Self <: Search[A]
  def self: Self
  def get(implicit ev: Self =:= Found[A]): A = self.a
}

case class Found[A](a: A) extends Search[A] {
  type Self = Found[A]
  def self = this
}
case class NotFound[A, Missing <: HList]() extends Search[A] {
  type Self = NotFound[A, Missing]
  def self = this
}


trait UnhappySearch {
  implicit def searchNotFound[A]: NotFound[A, A :: HNil] = NotFound()
}

object Search {
  type Aux[A, S] = Search[A] {type Self = S}
  def apply[A](implicit search: Search[A]): Aux[A, search.Self] = search

  implicit def searchFound[A](implicit instance: A): Found[A] = Found(instance)

  trait Mapping[A, B, S <: Search[A]] {
    type Out
    def apply(s: S, f: A => B): Out
  }

  object Mapping {
    type Aux[A, B, S <: Search[A], O] = Mapping[A, B, S] {type Out = O}

    implicit def mappingFound[A, B]: Aux[A, B, Found[A], Found[B]] = new Mapping[A, B, Found[A]] {
      type Out = Found[B]
      override def apply(s: Found[A], f: A => B): Out = Found(f(s.a))
    }

    implicit def mappingNotFound[A, B, M <: HList]: Aux[A, B, NotFound[A, M], NotFound[B, M]] = new Mapping[A, B, NotFound[A, M]] {
      type Out = NotFound[B, M]
      override def apply(s: NotFound[A, M], f: A => B): NotFound[B, M] = NotFound[B, M]()
    }
  }

  trait Mapping2[A, B, C, SA <: Search[A], SB <: Search[B]] {
    type Out
    def apply(sa: SA, sb: SB, f: (A, B) => C): Out
  }

  object Mapping2 {
    type Aux[A, B, C, SA <: Search[A], SB <: Search[B], O] = Mapping2[A, B, C, SA, SB] {type Out = O}


    implicit def bothFound[A, B, C]: Aux[A, B, C, Found[A], Found[B], Found[C]] =
      new Mapping2[A, B, C, Found[A], Found[B]] {
        type Out = Found[C]
        def apply(sa: Found[A], sb: Found[B], f: (A, B) => C) = Found(f(sa.a, sb.a))
      }

    implicit def leftFound[A, B, C, M <: HList]: Aux[A, B, C, Found[A], NotFound[B, M], NotFound[C, M]] =
      new Mapping2[A, B, C, Found[A], NotFound[B, M]] {
        type Out = NotFound[C, M]
        override def apply(sa: Found[A], sb: NotFound[B, M], f: (A, B) => C): NotFound[C, M] = NotFound[C, M]()
      }

    implicit def rightFound[A, B, C, M <: HList]: Aux[A, B, C, NotFound[A, M], Found[B], NotFound[C, M]] =
      new Mapping2[A, B, C, NotFound[A, M], Found[B]] {
        type Out = NotFound[C, M]
        override def apply(sa: NotFound[A, M], sb: Found[B], f: (A, B) => C): NotFound[C, M] = NotFound[C, M]()
      }

    implicit def nothingFound[A, B, C, MA <: HList, MB <: HList](implicit prepend: Prepend[MA, MB]): Aux[A, B, C, NotFound[A, MA], NotFound[B, MB], NotFound[C, prepend.Out]] =
      new Mapping2[A, B, C, NotFound[A, MA], NotFound[B, MB]] {
        type Out = NotFound[C, prepend.Out]
        override def apply(sa: NotFound[A, MA], sb: NotFound[B, MB], f: (A, B) => C): Out = NotFound[C, prepend.Out]()
      }
  }

}




