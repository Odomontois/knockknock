package meetup.graphs


import scala.language.experimental.macros
import scala.reflect.macros.whitebox


trait Interpret[U[_, _], T, I] {
  type Output
  def run(x: I): Output
  def as[T1]: Interpret.Aux[T1, I, Output] = this.asInstanceOf[Interpret.Aux[T1, I, Output]]
}

object Interpret {
  type Aux[T, I, O] = Interpret[T, I] {type Output = O}

  implicit def interpretMacro[T, I]: Interpret[T, I] = macro InterpretMacro.materialize[T, I]

  def initial[x, I](implicit term: Term[x, I]): Aux[Unit, I, term.Output] = new Interpret[Unit, I] {
    type Output = term.Output
    override def run(x: I): term.Output = term.run(x)
  }

  def combine[x, I] = new CombinePA[x, I]
  class CombinePA[x, I] {
    def apply[B](prefix: Aux[Unit, I, B])(implicit term: Term[x, B]): Aux[Unit, I, term.Output] = new Interpret[Unit, I] {
      type Output = term.Output
      override def run(x: I): term.Output = term.run(prefix.run(x))
    }
  }
}


object InterpretMacro {
  def materialize[T, I](c: whitebox.Context)(implicit tagT: c.WeakTypeTag[T], tagI: c.WeakTypeTag[I]): c.Tree = {
    import c.universe._

    val ConsTpe = typeOf[>>[_, _]].typeConstructor

    val T = weakTypeOf[T]
    val I = weakTypeOf[I]


    def unconsTpe(t: Type): List[Type] = t baseType ConsTpe.typeSymbol match {
      case TypeRef(_, sym, xs) if sym.asType.toType.typeConstructor == ConsTpe => xs.flatMap(unconsTpe(_))
      case _ => List(t)
    }

    val Int = q"_root_.meetup.graphs.Interpret"


    val names = Stream.from(0).map(i => c.freshName[TermName](s"interpret$i"))
    val namePairs = names.zip(None #:: names.map(Some(_)))
    val vals = unconsTpe(T).zip(namePairs).map {
      case (typ, (name, None)) => q"val $name = $Int.initial[$typ, $I]"
      case (typ, (name, Some(prev))) => q"val $name = $Int.combine[$typ, $I]($prev)"
    }
    val last = names(vals.length - 1)

    q"""{
       ..$vals

       new Interpret[$T, $I]{
         type Output = $last.Output
         def run(x: $I): Output = $last.run(x)
       }
      }"""
  }
}