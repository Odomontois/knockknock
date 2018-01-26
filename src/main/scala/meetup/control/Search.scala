package meetup.control

import meetup.control.Search.{Aux, Found, NotFound, Result}
import shapeless._
import shapeless.ops.hlist.Prepend


trait Search[+A] {
  type Out[+a] <: Result[a]
  def result: Out[A]
}


trait UnhappySearch {
  implicit def searchNotFound[A](implicit lowPriority: LowPriority): Aux[A, NotFound[A :: HNil, ?]] = Search.instance(NotFound())
}

object Search extends UnhappySearch {
  trait !!![X] {
    def value[A]: A
  }

  sealed trait Result[+A]
  case class Found[+A](value: A) extends Result[A]
  case class NotFound[Missing <: HList, +A]() extends Result[A] {
    def value(implicit missing: !!![Missing]): A = missing.value[A]
  }

  type Aux[+A, O[+a] <: Result[a]] = Search[A] {type Out[a] = O[a]}
  def apply[A](implicit search: Search[A]): Aux[A, search.Out] = search

  def instance[R[+a] <: Result[a], A](x: R[A]): Aux[A, R] = new Search[A] {
    override type Out[+a] = R[a]
    override def result: R[A] = x
  }

  implicit def searchFound[A](implicit a: A): Aux[A, Found] = instance(Found(a))

  trait Mapping[R[x] <: Result[x]] {
    def apply[A, B](s: R[A])(f: A => B): R[B]
  }

  object Mapping {
    implicit def mappingFound: Mapping[Found] = new Mapping[Found] {
      override def apply[A, B](s: Found[A])(f: A => B): Found[B] = Found(f(s.value))
    }

    implicit def mappingNotFound[M <: HList]: Mapping[NotFound[M, ?]] = new Mapping[NotFound[M, ?]] {
      override def apply[A, B](s: NotFound[M, A])(f: A => B): NotFound[M, B] = NotFound[M, B]()
    }
  }

  trait Mapping2[R[x] <: Result[x], U[x] <: Result[x]] {
    type Out[+C] <: Result[C]
    def apply[A, B, C](r: R[A], u: U[B])(f: (A, B) => C): Out[C]
  }

  object Mapping2 {
    type Aux[R[+x] <: Result[x], U[+x] <: Result[x], O[+x] <: Result[x]] = Mapping2[R, U] {type Out[+C] = O[C]}


    implicit def bothFound: Aux[Found, Found, Found] =
      new Mapping2[Found, Found] {
        type Out[+C] = Found[C]
        def apply[A, B, C](sa: Found[A], sb: Found[B])(f: (A, B) => C): Found[C] = Found(f(sa.value, sb.value))
      }

    implicit def leftFound[M <: HList]: Aux[Found, NotFound[M, +?], NotFound[M, +?]] =
      new Mapping2[Found, NotFound[M, ?]] {
        type Out[+C] = NotFound[M, C]
        override def apply[A, B, C](sa: Found[A], sb: NotFound[M, B])(f: (A, B) => C): NotFound[M, C] = NotFound[M, C]()
      }

    implicit def rightFound[M <: HList]: Aux[NotFound[M, +?], Found, NotFound[M, +?]] =
      new Mapping2[NotFound[M, ?], Found] {
        type Out[+C] = NotFound[M, C]
        override def apply[A, B, C](sa: NotFound[M, A], sb: Found[B])(f: (A, B) => C): NotFound[M, C] = NotFound[M, C]()
      }

    implicit def nothingFound[MA <: HList, MB <: HList](implicit prepend: Prepend[MA, MB]): Aux[NotFound[MA, +?], NotFound[MB, +?], NotFound[prepend.Out, +?]] =
      new Mapping2[NotFound[MA, ?], NotFound[MB, ?]] {
        type Out[+C] = NotFound[prepend.Out, C]
        override def apply[A, B, C](sa: NotFound[MA, A], sb: NotFound[MB, B])(f: (A, B) => C): Out[C] = NotFound[prepend.Out, C]()
      }
  }

}




