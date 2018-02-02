package meetup.graphs

import meetup.control.ValueOf
import meetup.graphs.Interpret.{runProg}
import meetup.graphs.console._
import meetup.graphs.lib._
import dsl._

object prog1 {

  type Program1[name <: String] = exec[readLine[name] >> putLine[name]]
  type readVar[name <: String] = readLine[name] ->> name
  type putVar[name <: String] =   do_[putLine[name] <<- name]

  type Program2[name <: String] = exec[readVar[name] >> putVar[name]]

  implicit val nameVal: ValueOf["name"] = ValueOf("name")
  implicit val ageVal: ValueOf["age"] = ValueOf("age")
  implicit val fullNameVal: ValueOf["full name"] = ValueOf("full name")
  implicit val firstNameVal: ValueOf["first name"] = ValueOf("first name")
  implicit val lastNameVal: ValueOf["last name"] = ValueOf("last name")
  implicit val lolVal: ValueOf["lol"] = ValueOf("lol")
  implicit val spaceVal: ValueOf[" "] = ValueOf(" ")
  implicit val midVal: ValueOf["mid"] = ValueOf("mid")
  implicit val concatSpaceVal: ValueOf["concatSpace"] = ValueOf("concatSpace")
  implicit val lollifyVal: ValueOf["lollify"] = ValueOf("lollify")
  implicit val xVal: ValueOf["x"] = ValueOf("x")
  implicit val yVal: ValueOf["y"] = ValueOf("y")

  type Program3[name <: String, age <: String] = exec[
    readVar[name] >>
    readVar[age] >>
    putVar[name] >>
    putVar[age] >>
    putVar[name]
  ]


  type concatWith[x, y, sep] =
    (const[sep] ->> "sep") >>
    ((concat <<- (x, "sep")) ->> "mid") >>
    ((concat <<- ("mid", y)))


  type Program4 = exec[
      readVar["first name"] >>
      readVar["last name"] >>
      (concatWith["first name", "last name", " "] ->> "full name") >>
      putVar["full name"]
  ]


  type Program5 = exec[
    defun["lollify", "x", String, (const["lol"] ->> "y") >> (concat <<- ("x", "y"))] >>
    readVar["age"] >>
    do_[apply1["lollify", "age"] >> putLine["name"]]
  ]

  type Program6 = exec[
    readVar["first name"] >>
    readVar["last name"] >>
    defun2["concatSpace", "x", String,  "y", String,
     ( const["lol"] ->> "sep") >>
     ((concat <<- ("x", "sep")) ->> "mid") >>
     ( concat <<- ("y", "mid"))] >>
    (apply1["concatSpace", "last name"] ->> "lol")
//    (apply1["lol", "last name"] ->> "full name")
  ]



  def main(args: Array[String]): Unit = {
//    runProg[T = Program1["name"]]()
//    runProg[T = Program2["name"]]()
//    runProg[T = Program3["name", "age"]]()


//    runProg[T = Program4]()
    runProg[T = Program6]()
//    implicitly
  }
}
