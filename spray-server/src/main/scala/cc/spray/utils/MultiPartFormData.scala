package cc.spray.utils

import cc.spray.http.HttpContent

case class MultiPartFormData(parts: List[BodyPart]) {
  def apply(name : String) = get(name).get
  def get(name : String) = parts.find(_.name == Some(name)) // Should we memoize here?
}

case class BodyPart(name : Option[String], content : HttpContent)
