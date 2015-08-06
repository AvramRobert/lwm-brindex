package indexer


import java.net.URLEncoder

import commands.{Command, CommandLineParser}
import f.ConverterLike
import play.api.libs.json.{JsObject, JsValue, Json}
import schema.AbstractSchema
import trans.Transition._
import trans.{Failure, Success}

import scala.util.Try
import scalaj.http.{HttpRequest, Http, HttpResponse}

object BranchIndexer {

  def commandLineParse: Transition[String, Vector[Command]] = s => CommandLineParser.parse(s)

  def json()(implicit cl: ConverterLike[String, JsValue]): Transition[String, JsValue] = s => Try(cl.convert(s)) match {
    case scala.util.Success(js) => Success(js)
    case scala.util.Failure(e) => Failure(e.getMessage)
  }


  def fromJson(): Transition[JsValue, String] = js => {
    js.asOpt[JsObject] match {
      case Some(jsObj) => Success {
        jsObj.value.map {
          case (s, v) => String.format("%10s  %s", (v \ "index").as[String], s"${(v \ "task").as[String]} [${(v \ "status").as[String]}]")
        }.mkString("\n")
      }
      case None => Failure("No viable input received while parsing")
    }
  }

  def postJson[B](uri: String)(implicit cl: ConverterLike[String, B]): Transition[JsValue, B] = js => {
    (request(uri).map(_
      .postData(js.toString())
      .header("content-type", "application/json")
      .asString) ~> mapTo[B]).contained("")
  }

  def putJson[B](uri: String)(implicit cl: ConverterLike[String, B]): Transition[JsValue, B] = js => {
    (request(uri).map(_
      .postData(js.toString())
      .method("PUT")
      .header("content-type", "application/json")
      .asString) ~> mapTo[B]).contained("")
  }

  def get[B](uri: String)(implicit cl: ConverterLike[String, B]): Transition[String, B] = s => {
    (request(uri).map(_.asString) ~> mapTo[B]).contained(s)
  }


  def mapTo[B](implicit cl: ConverterLike[String, B]): Transition[HttpResponse[String], B] = {
    case HttpResponse(body, code, headers) =>
      if (code == 200) Success(cl.convert(body))
      else Failure(s"Connection failure: $code " + cl.convert(body))
    case _ => Failure("Problem with connection")
  }

  def request(uri: String): Transition[String, HttpRequest] = s => success(Http(uri + s))(s)

  def sconcat[A](implicit schema: AbstractSchema[String, String => A]): Transition[String, Vector[A]] =  BranchIndexer.commandLineParse.map { v =>
    import schema._
    schema.rules.map { rule =>
      rule._1 -> v.find(_.parameter == rule._1).map(c => rule._2(c.value))
    }.filter(_._2.nonEmpty).toVector.sortBy(_._1).map(_._2.get)
}


trait PartialSyntaxChecker extends Transient[Command, String] with Schematic[String, Set[String]] {

  override def transition: Transition[Command, String] = cmd => {
    CommandLineParser.parse(cmd.value).flatMap { v =>
      if (v.size < schema.rules(cmd.parameter).size) Failure(s"Insufficient parameters for method ${cmd.parameter}. Please supply ${schema.rules(cmd.parameter).foldLeft("")((l, r) => l + " " + "-" + r)} with appropriate values.")
      else if (v.size > schema.rules(cmd.parameter).size) Failure(s"Surplus of parameters for method ${cmd.parameter}. Please supply only ${schema.rules(cmd.parameter).mkString(" ")} with appropriate values.")
      else if (v.map(_.parameter).toSet == schema.rules(cmd.parameter)) Success(cmd.value)
      else Failure(s"Invalid parameters for method ${cmd.parameter}. Please supply ${schema.rules(cmd.parameter).mkString(" ")} with appropriate values.")
    }
  }
}


object code extends Transient[String, String] with Schematic[String, String => String] {
  override implicit val schema: AbstractSchema[String, (String) => String] = new AbstractSchema[String, (String) => String] {
    override def rules: Map[String, (String) => String] = Map(
      ("r", s => "\"role\": \"" + s + "\" "),
      ("c", s => "\"category\": \""+ s +"\" "),
      ("task", s => "\"task\": \""+ s +"\" "),
      ("status", s => "\"status\":\""+ s +"\"")

    )

    override def order: IndexedSeq[String] = IndexedSeq("r", "c", "task", "status")
  }

  override def transition: Transition[String, String] = sconcat.map(_.mkString("{", "," , "}"))

}

object find extends Transient[String, String] with Schematic[String, String => String] {
  override implicit val schema: AbstractSchema[String, (String) => String] = new AbstractSchema[String, String => String] {
    override def rules: Map[String, (String) => String] = Map(
      ("p", s => s"?param=${URLEncoder.encode(s, "UTF-8")}")
    )

    override def order: IndexedSeq[String] = IndexedSeq()
  }

  override def transition: Transition[String, String] = sconcat.map(_.mkString(""))
  }


  object edit extends Transient[String, String] with Schematic[String, String => String] {

    override implicit val schema: AbstractSchema[String, (String) => String] = new AbstractSchema[String, (String) => String] {
      override def rules: Map[String, (String) => String] = Map(
        ("index", s => "\"index\": \"LWM-"+ s +"\" "),
        ("task", s => "\"task\": \""+ s +"\" "),
        ("status", s => "\"status\":\""+ s +"\"")
      )

      override def order: IndexedSeq[String] = IndexedSeq("index", "task", "status")
    }

    override def transition: Transition[String, String] = sconcat.map(_.mkString("{", ",", "}"))

  }

}


