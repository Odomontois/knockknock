package meetup.graphs

import meetup.control.ValueOf
import meetup.graphs.Interpret.{runProg}
import meetup.graphs.console._
import meetup.graphs.lib._
import dsl._

object prog1 {
  type Name = "name"
  type Age = "age"
  type FirstName = "first name"
  type LastName = "last name"
  type FullName = "full name"
  type Lollify = "lollify"
  type Lol = "lol"
  type Space = " "
  type Mid = "mid"
  type X = "x"
  type Y = "y"
  type ConcatSpace = "concatSpace"

  type Sep = "sep"

  type Program1[name <: String] = exec[readLine[name] >> putLine[name]]
  type readVar[name <: String] = readLine[name] ->> name
  type putVar[name <: String] =   do_[putLine[name] <<- name]
  type Program2[name <: String] = exec[readVar[name] >> putVar[name]]

  implicit val nameVal: ValueOf[Name] = ValueOf("name")
  implicit val ageVal: ValueOf[Age] = ValueOf("age")
  implicit val fullNameVal: ValueOf[FullName] = ValueOf("full name")
  implicit val firstNameVal: ValueOf[FirstName] = ValueOf("first name")
  implicit val lastNameVal: ValueOf[LastName] = ValueOf("last name")
  implicit val lolVal: ValueOf[Lol] = ValueOf("lol")
  implicit val spaceVal: ValueOf[Space] = ValueOf(" ")
  implicit val midVal: ValueOf[Mid] = ValueOf("mid")
  implicit val concatSpaceVal: ValueOf[ConcatSpace] = ValueOf("concatSpace")
  implicit val lollifyVal: ValueOf[Lollify] = ValueOf("lollify")
  implicit val xVal: ValueOf[X] = ValueOf("x")
  implicit val yVal: ValueOf[Y] = ValueOf("y")

  type Program3[name <: String, age <: String] = exec[
    readVar[name] >>
    readVar[age] >>
    putVar[name] >>
    putVar[age] >>
    putVar[name]
  ]


  type concatWith[x, y, sep] =
    (Sep := const[sep]) >>
    ((concat $ (x, Sep)) ->> Mid) >>
    ((concat $ (Mid, y)))


  type Program4 = exec[
      readVar[FirstName] >>
      readVar[LastName] >>
      (concatWith[FirstName, LastName, Space] ->> FullName) >>
      putVar[FullName]
  ]


  type Program5 = exec[
    defun[Lollify, X, String,
      (Y := const[Lol]) >>
      (concat $ (X, Y))] >>
    readVar[Age] >>
    do_[(Lollify #@ Age) >>
        putLine[Name]]
  ]

  type Program6 = exec[
    readVar[FirstName] >>
    readVar[LastName] >>
    defun2[ConcatSpace, X, String,  Y , String,
     ( Sep := const[Space]) >>
     ((concat $ (X, Sep)) ->> Mid) >>
     ( concat $ (Y, Mid))]
//    (FullName := apply2[ConcatSpace, FirstName,  LastName])
   ]



  def main(args: Array[String]): Unit = {

    6 match {
      case 1 => runProg[T = Program1["name"]]()
      case 2 => runProg[T = Program2["name"]]()
      case 3 => runProg[T = Program3["name", "name"]]()
      case 4 => runProg[T = Program4]()
      case 5 => runProg[T = Program5]()
      case 6 => runProg[T = Program6]()
    }
  }
}
