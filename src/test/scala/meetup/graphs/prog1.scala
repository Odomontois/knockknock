package meetup.graphs

import meetup.control.{RCons, RNil, Record, ValueOf}
import meetup.graphs.runners._
import meetup.graphs.console._
import meetup.graphs.dsl._
import meetup.graphs.lib._
import Record.field

import scala.reflect.runtime.universe._
import Record.RNil

object prog1 {

  type readVar[name <: String] =  readLine[name] ->> name
  type putVar[name <: String] =   do_[putLine[name] <<- name]

  type Program1[name <: String] = readLine[name] >> putLine[name]
  type Program2[name <: String] = readVar[name] >> putVar[name]

  type Program3[name <: String, age <: String] =
    readVar[name] >>
    readVar[age] >>
    putVar[name] >>
    putVar[age] >>
    putVar[name]


  type concatWith[x, y, sep] =
    (const[sep] ->> "sep") >>
    ((concat <<- (x, "sep")) ->> "mid") >>
    ((concat <<- ("mid", y)))


  type Program4 =
      readVar["first name"] >>
      readVar["last name"] >>
      (concatWith["first name", "last name", " "] ->> "full name") >>
      putVar["full name"]


  type Program5 =
    defun["lollify", "x", String, (const["lol"] ->> "y") >> (concat <<- ("x", "y"))] >>
//    printVars >>
    readVar["age"] >>
    do_[apply1["lollify", "age"] >> putLine["name"]]

  type Program6 =
  readVar["first name"] >>
  readVar["last name"] >>
  defun2["concatSpace", "x", String,  "y", String,
    (const[" "] ->> "sep") >>
    ((concat <<- ("x", "sep")) ->> "mid") >>
    (concat <<- ("mid", "y"))] >>
  (apply2["concatSpace", "first name", "last name"] ->> "full name")  >>
  putVar["full name"]
  //     do_[printVars]

  type Program7 =
    readVar["first name"] >>
    readVar["last name"] >>
    defun2["concatSpace", "x", String,  "y", String,
    (const[" "] ->> "sep") >>
    ((concat <<- ("x", "sep")) ->> "mid") >>
    (concat <<- ("mid", "y"))] >>
    (apply2["concatSpace", "first name", "last name"] ->> "full name")  >>
    putVar["full name"]
  //     do_[printVars]




  def main(args: Array[String]): Unit = {
//    runProgT[Program3["name", "age"]].apply()
//    Interpret.interpretMacro[Program3["name", "age"], RNil.type]
//    val u = Interpret.interpretMacro[readLine["name"] >> putLine["name"], RNil.type]
//    println(weakTypeOf[u.Output].dealias.widen.dealias)

    withDisplayOut[Program6].run()

//    Interpret.interpretMacro[Term.Aux,Program1["name"], RNil]
//    withTypeTag[Program1["name"]].run()
  }
}
