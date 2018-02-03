package meetup.graphs


trait >>[A, B]
trait ||[A, B]
trait ->>[A, B]
trait <<-[A, B]

trait exec[prog]
trait do_[prog]
trait const[prog]

trait get[name]

trait define[pname, ptype, expr]
trait call

object dsl {
  type defun[fname, pname, ptype, expr] = (define[pname, ptype, expr] ->> fname)
  type apply1[fname, pname] = (get[fname], get[pname]) >> call

  type defun2[fname, pname, ptype, pname2, ptype2, expr] = define[pname, ptype, define[pname2, ptype2, expr]] ->> fname
  type apply2[fname, pname1, pname2] = (apply1[fname, pname1], get[pname2]) >> call

  type defun3[fname, pname, ptype, pname2, ptype2, pname3, ptype3, expr] = (define[pname, ptype, define[pname2, ptype2, define[pname3, ptype3, expr]]] ->> fname)
  type apply3[fname, pname1, pname2, pname3] = (apply2[fname, pname1, pname2], get[fname]) >> call

  type :=[A, B] = B ->> A
  type $[A, B]  = A <<- B
}

object console {
  trait readLine[name]
  trait putLine[name]
  trait printVars
}

object lib {
  trait concat
}
