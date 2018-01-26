package meetup.graphs

import meetup.graphs.console._
import shapeless._
import shapeless.labelled._
import shapeless.syntax._

import scala.io.StdIn

object program2 extends App {
  trait |*|[x] { self =>
    type Input
    type Output
    def run: Input => Output

    type Self = |*|[x] {
      type Input = self.Input
      type Output = self.Output
    }
    def inputName(name: String): Self
    def outputName(name: String): Self
  }

  case class interpret[x, I, O]
  (runWithNames: (Option[String], Option[String]) => I => O,
   input: Option[String] = None,
   output: Option[String] = None) extends |*|[x] {
    type Input = I
    type Output = O
    override def run: I => O = runWithNames(input, output)
    override def inputName(name: String): interpret[x, I, O] = copy(input = Some(name))
    override def outputName(name: String): interpret[x, I, O] = copy(output = Some(name))
  }

  trait Constr {
    type F[O]
  }
  trait |**|[x, I] extends Constr {
    type F[O] = |*|[x] {type Input = I; type Output = O}
  }


  type #>[C <: Constr, O] = C#F[O]

  implicit def readLineProg: readLine |**| Unit #> String =
    interpret((name, _) => _ => StdIn.readLine(s"input ${name.getOrElse("something")}:\n"))

  implicit def putLineProg: putLine |**| String #> Unit =
    interpret((_, name) => value => println(s"[${name.getOrElse("???")}] $value"))

  implicit def consProg[x, y, A, B, C](implicit x: x |**| A #> B, y: y |**| B #> C): (x >> y) |**| A #> C =
    interpret((_, _) => input => y.run(x.run(input)))

  type Program1 = readLine >> putLine

  //  the[|*|[Program1]].run(())


  implicit def nameOutputProg[x, Vars <: HList, name <: String, Out]
  (implicit x: x |**| Unit #> Out,
   update: ops.record.Updater[Vars, FieldType[name, Out]],
   w: Witness.Aux[name]): (x -> name) |**| Vars #> update.Out =
    interpret((_, _) => vars => update(vars, field[name](x.inputName(w.value).run(()))))

  implicit def nameInputProg[x, name <: String, In, Vars <: HList]
  (implicit x: x |**| In #> Unit,
   select: ops.record.Selector.Aux[Vars, name, In],
   w: Witness.Aux[name]): (name -> x) |**| Vars #> Vars =
    interpret((_, _) => vars => {x.outputName(w.value).run(select(vars)); vars})

  case class exec[prog]()

  implicit def execProg[prog, Out](implicit prog: prog |**| HNil #> Out): exec[prog] |**| Unit #> Unit =
    interpret((_, _) => _ => prog.run(HNil))

  type Program2 = exec[readLine -> "name"]

//  the[|*|[Program2] ].run(())

  type Program3 = exec[(readLine -> "name") >>
                       (readLine -> "age") >>
                       ("name" -> putLine) >>
                       ("age" -> putLine)]
  the[|*|[Program3]].run(())


}
