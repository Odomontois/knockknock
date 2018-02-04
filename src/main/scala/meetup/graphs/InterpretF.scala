package meetup.graphs

import scala.language.experimental.macros
import scala.reflect.macros.whitebox


trait InterpretF[U[_, _[_], _, _[_], _], T, F[_], I] {
  type FOut[_]
  type VOut
  val value: U[T, F, I, FOut, VOut]
}

object InterpretF {
  type Aux[U[_, _[_], _, _[_], _], T, F[_], I, G[_], O] = InterpretF[U, T, F, I] {
    type VOut = O
    type FOut[a] = G[a]
  }

  implicit def interpretMacro[U[_, _[_], _, _[_], _], T, F[_], I](implicit int: InterpreterF[U]): InterpretF[U, T, F, I] = macro InterpretFMacro.materialize[U, T, F, I]
}


object InterpretFMacro {
  type Q[a] = a
  def materialize[U[_, _[_], _, _[_], _], T, F[_], I](c: whitebox.Context)(int: c.Tree)(
    implicit tagT: c.WeakTypeTag[T],
    tagI: c.WeakTypeTag[I],
    tagU: c.WeakTypeTag[U[_, Q, _, Q, _]],
    tagF: c.WeakTypeTag[F[_]]): c.Tree = {
    import c.universe._

    val ConsTpe = typeOf[>>[_, _]].typeConstructor

    val T = weakTypeOf[T]
    val I = weakTypeOf[I]
    val U = weakTypeOf[U[_, Q, _, Q, _]].typeConstructor
    val F = weakTypeOf[F[_]].typeConstructor


    def unconsTpe(t: Type): List[Type] = t baseType ConsTpe.typeSymbol match {
      case TypeRef(_, sym, xs) if sym.asType.toType.typeConstructor == ConsTpe => xs.flatMap(unconsTpe(_))
      case _ => List(t)
    }


    val Int = c.freshName[TermName]("interpreter")
    val IntT = appliedType(weakTypeOf[InterpreterF[U]].typeConstructor, U)


    val names = Stream.from(0).map(i => c.freshName[TermName](s"interpret$i"))
    val namePairs = names.zip(None #:: names.map(Some(_)))
    val vals = unconsTpe(T).zip(namePairs).map {
      case (typ, (name, None)) => q"val $name = $Int.init[$typ, $F, $I]()"
      case (typ, (name, Some(prev))) => q"val $name = $Int.combine[$typ]($prev)"
    }
    val myInstance = c.freshName[TermName]("myInstance")

    //    c.info(c.enclosingPosition, vals.toString, false)
    //    c.info(c.enclosingPosition, T.toString, false)
    //    c.info(c.enclosingPosition, unconsTpe(T)toString, false)
    val last = names(vals.length - 1)

    q"""{
       val $Int = $int

       ..$vals

       val $myInstance = $last.as[$T]

       new InterpretF[$U, $T, $F, $I]{
         type FOut[x] = $myInstance.FOut[x]
         type VOut = $myInstance.VOut
         val value = $myInstance
       }
      }"""
  }
}

trait InterpreterF[U[_, _[_], _, _[_], _]] {
  def init[x, F[_], I]: Any
  def combine[x]: Any
}

