package meetup.graphs
import meetup.control.Search
import meetup.graphs.console._
import shapeless.labelled.{FieldType, field}
import shapeless._

import scala.io.StdIn

object program3 extends App {
  trait |*|[x] {
    self =>
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


  type |**|[x, I, O] = |*|[x] {
    type Input = I
    type Output = O
  }

  implicit def readLineProg: |**|[readLine, Unit, String] =
    interpret((name, _) => _ => StdIn.readLine(s"input ${name.getOrElse("something")}:\n"))

  implicit def putLineProg: |**|[putLine, String , Unit] =
    interpret((_, name) => value => println(s"[${name.getOrElse("???")}] $value"))

  type Program1 = readLine >> putLine

  //  the[|*|[Program1]].run(())


  implicit def nameOutputProg[x, Vars <: HList, name <: String, Out]
  (implicit x: |**|[x,  Unit , Out],
   update: ops.record.Updater[Vars, FieldType[name, Out]],
   w: Witness.Aux[name]):  |**|[(x -> name), Vars , update.Out] =
    interpret((_, _) => vars => update(vars, field[name](x.inputName(w.value).run(()))))

  implicit def nameInputProg[x, name <: String, In, Vars <: HList]
  (implicit x: |**|[x,  In, Unit],
   select: ops.record.Selector.Aux[Vars, name, In],
   w: Witness.Aux[name]):  |**|[name -> x, Vars , Vars] =
    interpret((_, _) => vars => {
      x.outputName(w.value).run(select(vars)); vars
    })

  case class exec[prog]()

  implicit def execProg[prog, Out](implicit prog: |**|[prog , HNil, Out]):  |**|[exec[prog],  Unit , Unit] =
    interpret((_, _) => _ => prog.run(HNil))

  type Program2 = exec[readLine -> "name"]

  //    the[|*|[Program2] ].run(())

  type Program3 = exec[
    (readLine -> "name") >>
    (readLine -> "age") >>
    ("name" -> putLine) >>
    ("age" -> putLine)
  ]




  case class rename()


  implicit def consProgSearch[x, y, A, B, C, Rx[+a] <: Search.Result[a], Ry[+a] <: Search.Result[a]]
  (implicit xs: Search.Aux[|**|[x, A, B], Rx],
   ys: Search.Aux[|**|[y, B, C], Ry],
   combo: Search.Mapping2[Rx, Ry]): Search.Aux[|**|[x >> y, A, C], combo.Out] =
    Search.instance[combo.Out,|**|[x >> y, A, C]](combo(xs.result, ys.result)((x, y) => interpret[x >> y, A, C]((_, _) => input => y.run(x.run(input)))))



  the[Search[|*|[readLine >> putLine]]].result.value


//  consProgSearch[readLine, putLine, Unit, String, Unit, Search.Found, Search.Found] : Search[|*|[readLine >> putLine]]
}

