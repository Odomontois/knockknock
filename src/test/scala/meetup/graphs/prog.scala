package meetup.graphs

import meetup.graphs.console._
import meetup.graphs.dsl._
import meetup.graphs.lib._


object prog {

  type Name = "name"
  type Age = "age"
  type FirstName = "first name"
  type LastName = "last name"
  type FullName = "full name"
  type Space = " "
  type Lollify = "lollify"
  type X = "x"
  type Y = "y"
  type Lol = " lol"
  type Word = "word"
  type Result = "result"
  type Sep = "sep"
  type Mid = "mid"

  type readVar[name <: String] =  name := readLine[name]
  type putVar[name <: String] =   do_[putLine[name] $ name]

  type Program1[name <: String] = readLine[name] >> putLine[name]
  type Program1A[name <: String] = readLine[name]


  type Program2[name <: String] = readVar[name] >> putVar[name]

  type Program3[name <: String, age <: String] =
    readVar[name] >>
      readVar[age] >>
      putVar[name] >>
      putVar[age] >>
      putVar[name]




  type concatWith[x, y, sep] =
      (Sep := const[sep]) >>
      (Mid := (concat $ (x, Sep))) >>
      ((concat $ (Mid, y)))


  type Program4 =
      readVar[FirstName] >>
      readVar[LastName] >>
      (FullName := concatWith[FirstName, LastName, Space]) >>
      putVar[FullName]

  type Program5 =
    defun[Lollify, X, String,
        (Y := const[Lol]) >>
        (concat $ (X, Y))] >>
      readVar[Word] >>
      do_[apply1[Lollify, Word] >>
          putLine[Result]]

  type ConcatSpace = "concatSpace"

  type Program6 =
    readVar[FirstName] >>
      readVar[LastName] >>
      defun2[ConcatSpace, X, String,  Y, String,
        (const[Space] ->> Sep) >>
          ((concat <<- (X, Sep)) ->> Mid) >>
          (concat <<- (Mid, Y))] >>
      (apply2[ConcatSpace, FirstName, LastName] ->> FullName)  >>
      putVar[FullName]

  type Program7 =
    readVar[FirstName] >>
      readVar[LastName] >>
      defun2[ConcatSpace, X, String,  Y, String,
        (const[Space] ->> Sep) >>
          ((concat <<- (X, Sep)) ->> Mid) >>
          (concat <<- (Mid, Mid))] >>
      (apply2[ConcatSpace, FirstName, LastName] ->> FullName)  >>
      putVar[FullName]

}
