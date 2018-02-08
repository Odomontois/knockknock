package meetup.graphs

import cats.arrow.FunctionK
import cats.~>
import meetup.control.{LiftRecF, RecordF}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox


trait InterpretF[T, F[_], I] {
  type FOut[_]
  type VOut
  val value: Monadic.Aux[T, F, I, FOut, VOut]
}

object InterpretF {
  type Aux[T, F[_], I, G[_], O] = InterpretF[T, F, I] {
    type VOut = O
    type FOut[a] = G[a]
  }
  def apply[T, F[_], I](implicit int: InterpretF[T, F, I]): Aux[T, F, I, int.FOut, int.VOut] = int

  implicit def interpretMacro[T, F[_], I]: InterpretF[T, F, I] = macro InterpretFMacro.materialize[T, F, I]
}

trait ReadVar[T, F[_], I <: RecordF[F], K]{
  type FOut[a]
  type ROut <: RecordF[FOut]
  type VOut
}


class InterpretFMacro(val c: whitebox.Context) {
  import c.universe._
  type Q[a] = a
  def materialize[T, F[_], I](
    implicit tagT: c.WeakTypeTag[T],
    tagI: c.WeakTypeTag[I],
    tagF: c.WeakTypeTag[F[_]]): c.Tree = {


    val ConsTpe = typeOf[>>[_, _]].typeConstructor

    val T = weakTypeOf[T]
    val I = weakTypeOf[I]
    val MonadicSym = weakTypeOf[Monadic[_, Q, _]].typeConstructor.typeSymbol.companion
    val F = weakTypeOf[F[_]].typeConstructor
    val FK = weakTypeOf[FunctionK[Q, Q]].typeConstructor


    def unconsTpe(t: Type): List[Type] = t baseType ConsTpe.typeSymbol match {
      case TypeRef(_, sym, xs) if sym.asType.toType.typeConstructor == ConsTpe => xs.flatMap(unconsTpe(_))
      case _ => List(t)
    }

    val Int = c.freshName[TermName]("interpreter")


    val names = Stream.from(0).map(i => c.freshName[TermName](s"interpret$i"))
    val namePairs = names.zip(None #:: names.map(Some(_)))
    val vals = unconsTpe(T).zip(namePairs).map {
      case (typ, (name, None)) => q"val $name = $Int.init[$typ, $F, $I]()"
      case (typ, (name, Some(prev))) => q"val $name = $prev.andThen[$typ]"
    }
    val myInstance = c.freshName[TermName]("myInstance")

    val last = names(vals.length - 1)

    q"""{
       val $Int = $MonadicSym.interpreter

       ..$vals

       val $myInstance = $last.as[$T]


       new InterpretF[$T, $F, $I]{
         type FOut[x] = $myInstance.FOut[x]
         type VOut = $myInstance.VOut
         val value = $myInstance
       }
      }"""
  }
}

