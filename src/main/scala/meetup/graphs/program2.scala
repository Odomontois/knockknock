package meetup.graphs

import meetup.control._
import meetup.graphs.console._
import Record._

import scala.reflect.runtime.universe._
import scala.io.StdIn

object program2 extends App {
  trait |*|[x, Inp] { self =>
    type Output
    def run: Inp => Output

    type Self = |*|[x, Inp] {
      type Output = self.Output
    }
    def inputName(name: String): Self
    def outputName(name: String): Self
  }

  case class interpret[x, I, O]
  (runWithNames: (Option[String], Option[String]) => I => O,
   input: Option[String] = None,
   output: Option[String] = None) extends |*|[x, I] {
    type Output = O
    override def run: I => O = runWithNames(input, output)
    override def inputName(name: String): interpret[x, I, O] = copy(input = Some(name))
    override def outputName(name: String): interpret[x, I, O] = copy(output = Some(name))
  }


  type |**|[x, Inp, O] = |*|[x, Inp] {type Output = O}


  implicit def readLineProg[X]: |**|[readLine, X, String] =
    interpret((name, _) => _ => StdIn.readLine(s"input ${name.getOrElse("something")}:\n"))

  implicit def putLineProg: |**|[putLine, String, Unit] =
    interpret((_, name) => value => println(s"[${name.getOrElse("???")}] $value"))

  implicit def consProg[x, y, A, B](implicit x: |**|[x, A, B], y: |*|[y, B]): |**|[x >> y, A, y.Output] =
    interpret((_, _) => input => y.run(x.run(input)))


  def runProg[T]()(implicit prog: |*|[T, Unit]): Unit = prog.run(())
  implicitly[|*|[readLine >> putLine, Unit]]

  //  runProg[readLine >> putLine]()


  implicit def nameOutputProg[x, Vars <: Record, name <: String, Out]
  (implicit x: |**|[x, Unit, Out],
   update: UpdateRec[name, Vars],
   name: ValueOf[name]): |**|[x -> name, Vars, update.Out[Out]] =
    interpret((_, _) => vars => update(x.inputName(name.value).run(vars), vars))

  implicit def nameInputProg[x, name <: String, In, Vars <: Record]
  (implicit x: |*|[x, In],
   select: SelectRec.Aux[name, Vars, In],
   name: ValueOf[name]): |**|[name -> x, Vars, Vars] =
    interpret((_, _) => vars => {
      x.outputName(name.value).run(select(vars))
      vars
    })

  implicit val nameVal: ValueOf["name"] = ValueOf("name")
  implicit val ageVal: ValueOf["age"] = ValueOf("age")

  case class exec[prog]()

  implicit def execProg[prog, Out](implicit prog: |*|[prog, RNil]): |**|[exec[prog], Unit, Unit] =
    interpret((_, _) => _ => {prog.run(RNil); ()})

  type Program2 = exec[readLine -> "name"]

//  nameOutputProg[readLine, RNil, "name", String]

  val u = implicitly[|*|[readLine -> "name", RNil]]
  implicitly[u.Output =:=  RCons["name", String, RNil]]

  implicitly[|*|[exec[readLine -> "name"], Unit]]

  runProg[Program2]()

  implicitly[|*|["name" -> putLine, u.Output]]
  implicitly[|*|[(readLine -> "name") >> ("name" -> putLine), RNil]]


  type Program3 = exec[
    (readLine -> "name") >>
    (readLine -> "age") >>
    ("name" -> putLine) >>
    ("age" -> putLine)
  ]

//  runProg[Program3]()


}
