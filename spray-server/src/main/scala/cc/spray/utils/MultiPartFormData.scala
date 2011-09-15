package cc.spray.utils

import cc.spray.http.HttpContent

case class MultiPartFormData(parts: List[BodyPart]) extends FieldSource {
  def apply(name : String) = get(name).get
  def get(name : String) = parts.find(_.name == Some(name)) // Should we memoize here?
  def fields = parts.filter(_.name.isDefined).map(part => part.name.get -> part.content).toMap  // Should we memoize here?
}

case class BodyPart(name : Option[String], content : HttpContent)
