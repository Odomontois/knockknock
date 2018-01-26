package meetup.control

trait Func[A, B] {
  def apply(x: A): B
}


