package meetup.graphs
import meetup.control.Record.RNil
import meetup.graphs.Term.runners._
import meetup.graphs.prog._

object termRunner {
  def main(args: Array[String]): Unit = {
    Interpret.interpretMacro[Term.Aux, Program1[Name], RNil]

    6 match {
             case 1 => withTypeTag[Program1[Name]].run()
             case 2 => withDisplayOut[Program2[Name]].run()
             case 3 => withDisplayOut[Program3[Name, Age]].run()
             case 4 => withDisplayOut[Program4].run()
             case 5 => withDisplayOut[Program5].run()
             case 6 => withDisplayOut[Program6].run()
      case _ =>
    }
  }
}
