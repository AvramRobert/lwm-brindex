package f

import play.api.libs.json.{Json, JsValue}


trait ConverterLike[A, B] {
  def convert(a: A): B
}

object ConverterLike {

  implicit object StringToJson extends ConverterLike[String, JsValue] {
    override def convert(a: String): JsValue = Json.parse(a)
  }
}
