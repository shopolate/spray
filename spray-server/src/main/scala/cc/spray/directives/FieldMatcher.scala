package cc.spray
package directives

import marshalling.{CantUnmarshal, UnmarshalWith}
import http.ContentTypeRange

private [directives] trait FieldMatcher[A] {
  def name : String

  def apply(fields: FieldMap): Either[FieldMatcherFailure, A]

  def ? = new OptionWrappingFieldMatcher(this)

  def ? [B :Unmarshaller](default: B) = new SimpleFieldMatcher[B](name, cc.spray.unmarshaller[B]) {
    override def notFound = Right(default)
  }

  def as[B :Unmarshaller] = new SimpleFieldMatcher[B](name, cc.spray.unmarshaller[B])

  def ! [B :Unmarshaller](requiredValue: B): RequiredFieldMatcher = { params =>
    new SimpleFieldMatcher[B](name, cc.spray.unmarshaller[B])
      .apply(params)
      .right
      .toOption == Some(requiredValue)
  }
}

private [directives] class SimpleFieldMatcher[A](val name: String, val unmarshaller: Unmarshaller[A]) extends FieldMatcher[A] {
  def apply(fields: FieldMap): Either[FieldMatcherFailure, A] = {
    fields.get(name) match {
      case Some(value) => unmarshaller(value.contentType) match {
        case UnmarshalWith(converter) => converter(value) match {
          case Right(convertedValue) => Right(convertedValue)
          case Left(MalformedRequestContentRejection(msg)) => Left(MalformedField(msg, name))
          case other => Left(MalformedField("Can not convert field", name))
        }
        case CantUnmarshal(onlyFrom) => Left(ContentIncompatibility(onlyFrom, name))
      }
      case None => notFound
    }
  }

  protected def notFound: Either[FieldMatcherFailure, A] = Left(MissingField(name))
}

private [directives] class OptionWrappingFieldMatcher[A](wrapped : FieldMatcher[A]) extends FieldMatcher[Option[A]] {
  val name = wrapped.name

  override def apply(fields: FieldMap): Either[FieldMatcherFailure, Option[A]] = wrapped.apply(fields) match {
    case Right(value) => Right(Some(value))
    case Left(MissingField(_)) => Right(None)
    case Left(other) => Left(other)
  }
}

private [directives] abstract sealed class FieldMatcherFailure
private [directives] case class MalformedField(msg : String, name : String) extends FieldMatcherFailure
private [directives] case class MissingField(name : String) extends FieldMatcherFailure
private [directives] case class ContentIncompatibility(onlyFrom: List[ContentTypeRange], name : String) extends FieldMatcherFailure
