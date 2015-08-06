package commands

import trans.{Failure, Success, Result}

import scala.util.Try
import scala.util.matching.Regex


case object CommandLineParser extends CommandParser

object CommandParser {
}

class CommandParser extends SimpleRegexParser[Command] {

  override def regex: Regex = """(?<=[-{}|/])(?<name>[a-zA-Z0-9]*)[ |:|"]*(?<value>[\w|.|?|=|&|+| |:|/|\\]*)(?=[ |"]|$)""".r

  override def ^^(i: String): Command = Command(i.split(" ")(0), i.split(" ").drop(1).map(_.filterNot(_ == '"')).mkString(" "))

}

trait SimpleRegexParser[+A] {
  def regex: Regex


  //TODO: FIX. The current scheme checks correctly but fails when passed a composed value that belongs to a single parameter but happens to be split by ` `
  def parse(s: String): Result[Vector[A]] =
    Try(regex findAllIn s toVector) match {
      case util.Success(v) =>
        if(v.map(_.split(" ")).forall(_.length > 1)) Success(v map ^^)
        else Failure("Insufficient keys or values provided. Please provide key-value pairs.")
      case util.Failure(e) => Failure(s"Parse error:\n ${e.getMessage}")
    }

  def ^^(i: String): A
}




