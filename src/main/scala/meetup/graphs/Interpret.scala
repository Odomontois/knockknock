package meetup.graphs


import scala.language.experimental.macros
import scala.reflect.macros.whitebox


trait Interpret[U[_, _, _], T, I] {
  type Output
  val value: U[T, I, Output]
}

object Interpret {
  type Aux[U[_, _, _], T, I, O] = Interpret[U, T, I] {type Output = O}

  implicit def interpretMacro[U[_, _, _], T, I](implicit int: Interpreter[U]): Interpret[U, T, I] = macro InterpretMacro.materialize[U, T, I]
}


object InterpretMacro {
  def materialize[U[_, _, _], T, I](c: whitebox.Context)(int: c.Tree)(implicit tagT: c.WeakTypeTag[T], tagI: c.WeakTypeTag[I], tagU: c.WeakTypeTag[U[_, _, _]]): c.Tree = {
    import c.universe._

    val ConsTpe = typeOf[>>[_, _]].typeConstructor

    val T = weakTypeOf[T]
    val I = weakTypeOf[I]
    val U = weakTypeOf[U[_, _, _]].typeConstructor


    def unconsTpe(t: Type): List[Type] = t baseType ConsTpe.typeSymbol match {
      case TypeRef(_, sym, xs) if sym.asType.toType.typeConstructor == ConsTpe => xs.flatMap(unconsTpe(_))
      case _ => List(t)
    }



    val Int = c.freshName[TermName]("interpreter")
    val IntT = appliedType(weakTypeOf[Interpreter[U]].typeConstructor, U)


    val names = Stream.from(0).map(i => c.freshName[TermName](s"interpret$i"))
    val namePairs = names.zip(None #:: names.map(Some(_)))
    val vals = unconsTpe(T).zip(namePairs).map {
      case (typ, (name, None)) => q"val $name = $Int.init[$typ, $I]()"
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

       new Interpret[$U, $T, $I]{
         type Output = $myInstance.Output
         val value = $myInstance
       }
      }"""
  }
}

trait Interpreter[U[_, _, _]]{
  def init[x, I]: Any
  def combine[x]: Any
}

object Interpreter{

}
