package meetup.graphs
import meetup.graphs.Term.runners._
import meetup.graphs.prog._

object termRunner {
  def main(args: Array[String]): Unit = {
     3 match {
       case 1 => withTypeTag[Program1["name"]].run()
       case 2 => withDisplayOut[Program2["name"]].run()
       case 3 => withDisplayOut[Program3["name", "age"]].run()
       case 4 => withDisplayOut[Program4].run()
       case 5 => withDisplayOut[Program5].run()
       case 6 => withDisplayOut[Program6].run()
     }
  }
}
