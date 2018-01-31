package meetup.graphs

import meetup.control._
import meetup.graphs.console._
import Record._

import scala.reflect.runtime.universe._
import scala.io.StdIn

object program2 extends App {
  trait Interpret[x] { self =>
    type Input
    type Output
    def run: Input => Output

    type Self = IAux[x, Input, Output]
    def inputName(name: String): Self
    def outputName(name: String): Self
  }

  type IAux[x, I, O] = Interpret[x] {
    type Input = I
    type Output = O
  }

  case class interpret[x, I, O]
  (runWithNames: (Option[String], Option[String]) => I => O,
   input: Option[String] = None,
   output: Option[String] = None) extends Interpret[x] {
    type Input = I
    type Output = O
    override def run: I => O = runWithNames(input, output)
    override def inputName(name: String): interpret[x, I, O] = copy(input = Some(name))
    override def outputName(name: String): interpret[x, I, O] = copy(output = Some(name))
  }


  implicit def readLineProg[X]: IAux[readLine, X, String] =
    interpret((name, _) => _ => StdIn.readLine(s"input ${name.getOrElse("something")}:\n"))

  implicit def putLineProg: IAux[putLine, String, Unit] =
    interpret((_, name) => value => println(s"[${name.getOrElse("???")}] $value"))

  implicit def consProg[x, y, A, B, C](implicit x: IAux[x, A, B], y: IAux[y, B, C]): IAux[x >> y, A, C] =
    interpret((_, _) => input => y.run(x.run(input)))


  def runProg[T, O]()(implicit prog: IAux[T, Unit, O]): Unit = runProgI(())
  def runProgI[T, I, O](x: I)(implicit prog: IAux[T, I, O]): Unit = prog.run(x)


  implicitly[Interpret[readLine >> putLine]]

//  runProg[T = readLine >> putLine] ()


  implicit def nameOutputProg[x, Vars <: Record, name <: String, Out, ROut <: Record]
  (implicit x: IAux[x, Unit, Out],
   name: ValueOf[name],
   update: UpdateRec.Aux[name, Vars, Out, ROut]): IAux[x -> name, Vars, ROut] =
    interpret((_, _) => vars => update(x.inputName(name.value).run(vars), vars))

  implicit def nameInputProg[x, name <: String, In, Out, Vars <: Record]
  (implicit select: SelectRec.Aux[name, Vars, In],
   x: IAux[x, In, Out],
   name: ValueOf[name]): IAux[x <<- name, Vars, Vars] =
    interpret((_, _) => vars => {
      x.outputName(name.value).run(select.apply(vars))
      vars
    })

  implicit val nameVal: ValueOf["name"] = ValueOf("name")
  implicit val ageVal: ValueOf["age"] = ValueOf("age")

  case class exec[prog]()

  implicit def execProg[prog, Out](implicit prog: IAux[prog, RNil, Out]): IAux[exec[prog], Unit, Unit] =
    interpret((_, _) => _ => {prog.run(RNil); ()})

  type Program2 = exec[readLine -> "name"]

  //  nameOutputProg[readLine, RNil, "name", String]


  //  runProgI[T = readLine -> "name", I = RNil](RNil)

  //  val u = implicitly[|*|[, RNil, RCons["name", String, RNil]]]

//  runProg[T = exec[readLine -> "name"]]()

//  runProgI[T = putLine <<- "name"](field[K ="name"]("Oleg") :: RNil)

  //
  //  runProg[Program2]()
  //
  //  implicitly[|*|["name" -> putLine, u.Output]]
  //  implicitly[|*|[(readLine -> "name") >> ("name" -> putLine), RNil]]


  type Program3 = exec[
    (readLine ->"name") >>
    (readLine -> "age" ) >>
    (putLine <<- "name") >>
    (putLine <<- "age")
  ]

  type Program3A = exec[
    (readLine ->"name") >>
    (readLine -> "age") >>
    (putLine <<- "age")
  ]

    runProg[T = Program3]()

//  val t = implicitly[UpdateRec["age", RCons["name", String, RNil]]]
//
//  implicitly[t.Out[String] =:= RCons["name", String, RCons["age", Int, RNil]]]


}
