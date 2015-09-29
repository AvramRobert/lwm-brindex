package commands

import indexer.BranchIndexer.PartialSyntaxChecker
import schema.AbstractSchema
import trans.{Failure, Success}
import trans.Transition.->

case class Command(parameter: String, value: String)

trait CommandLineApp extends App {


  override def main(args: Array[String]) = {
    run(args(0)) {
      Command(args(0), args.slice(1, args.length).mkString(" "))
    } match {
      case Success(s) => {
        println("Success:")
        println(s)
      }
      case Failure(e) => println(s"Failed:\n$e")
    }
  }

  def syntaxCheck: PartialSyntaxChecker = new PartialSyntaxChecker {
    override implicit val schema: AbstractSchema[String, Set[String]] = new AbstractSchema[String, Set[String]] {
      override def rules: Map[String, Set[String]] = commandSyntax

      override def order: IndexedSeq[String] = IndexedSeq()
    }
  }

  def commandSyntax: Map[String, Set[String]]

  def run: String => Command -> _
}
