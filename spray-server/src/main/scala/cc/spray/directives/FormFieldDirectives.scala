package cc.spray
package directives

import cc.spray.http.HttpContent
import cc.spray._
import marshalling.{UnmarshalWith, DefaultUnmarshallers}

private[spray] trait FormFieldDirectives {
  this: BasicDirectives =>

  private type FM[A] = FieldMatcher[A]

  /**
   * Returns a Route that rejects the request if a form field with the given name cannot be found.
   * If it can be found the form value is extracted and passed as argument to the inner Route building function.
   */
  def formField[A](pm: FM[A]): SprayRoute1[A] = filter1[A] { ctx =>
    extractFormContent(ctx) match {
      case Some(fields) =>
        pm(fields) match {
          case Right(value) => Pass.withTransform(value) {
            _.cancelRejections {
              _ match {
                case MissingFormFieldRejection(n) if n == pm.name => true
                case MalformedFormFieldRejection(_, Some(n)) if n == pm.name => true
                case _ => false
              }
            }
          }
          case Left(MissingField(x)) => Reject(MissingFormFieldRejection(x))
          case Left(MalformedField(error, param)) => Reject(MalformedFormFieldRejection(error, Some(param)))
          case Left(ContentIncompatibility(onlyFrom, _)) => Reject(UnsupportedRequestContentTypeRejection(onlyFrom))
        }
      case None=>
        Reject(MalformedRequestContentRejection("The request content does not contain a form of type 'application/x-www-form-urlencoded' nor 'multipart/form-data'."))
    }
  }

  private def extractFormContent(ctx : RequestContext) : Option[FieldMap] = ctx.request.content.map { content =>
    DefaultUnmarshallers.FormContentUnmarshaller(content.contentType) match {
      case UnmarshalWith(converter) => converter(content).right.toOption.map(_.fields)
      case _ => DefaultUnmarshallers.MultiPartFormDataUnmarshaller(content.contentType) match {
        case UnmarshalWith(converter) => converter(content).right.toOption.map(_.fields)
        case _ => None
      }
    }
  }.getOrElse(None)

  /**
   * Returns a Route that rejects the request if a form field with the given name cannot be found.
   * If it can be found the field value is extracted and passed as argument to the inner Route building function.
   */
  def formFields[A](a: FM[A]): SprayRoute1[A] = formField(a)

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B](a: FM[A], b: FM[B]): SprayRoute2[A, B] = {
    formFields(a) & formField(b)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C](a: FM[A], b: FM[B], c: FM[C]): SprayRoute3[A, B, C] = {
    formFields(a, b) & formField(c)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C, D](a: FM[A], b: FM[B], c: FM[C], d: FM[D]): SprayRoute4[A, B, C, D] = {
    formFields(a, b, c) & formField(d)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C, D, E](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E]): SprayRoute5[A, B, C, D, E] = {
    formFields(a, b, c, d) & formField(e)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C, D, E, F](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                   f: FM[F]): SprayRoute6[A, B, C, D, E, F] = {
    formFields(a, b, c, d, e) & formField(f)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C, D, E, F, G](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                      f: FM[F], g: FM[G]): SprayRoute7[A, B, C, D, E, F, G] = {
    formFields(a, b, c, d, e, f) & formField(g)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C, D, E, F, G, H](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                         f: FM[F], g: FM[G], h: FM[H]): SprayRoute8[A, B, C, D, E, F, G, H] = {
    formFields(a, b, c, d, e, f, g) & formField(h)
  }

  /**
   * Returns a Route that rejects the request if the form fields with the given names cannot be found.
   * If it can be found the field values are extracted and passed as arguments to the inner Route building function.
   */
  def formFields[A, B, C, D, E, F, G, H, I](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                            f: FM[F], g: FM[G], h: FM[H], i: FM[I]): SprayRoute9[A, B, C, D, E, F, G, H, I] = {
    formFields(a, b, c, d, e, f, g, h) & formField(i)
  }

  /**
   * Returns a Route that rejects the request if the form field with the given name cannot be found or does not
   * have the required value.
   */
  def formField(p: RequiredFieldMatcher) = filter { ctx =>
    extractFormContent(ctx) match {
      case Some(fields) if p(fields) => Pass()
      case _ => Reject()
    }
  }

  /**
   * Returns a Route that rejects the request if the form field with the given name cannot be found or does not
   * have the required value.
   */
  def formFields(p: RequiredFieldMatcher, more: RequiredFieldMatcher*) = {
      val allRFM = p +: more
      filter { ctx => if (allRFM.forall(_(extractFormContent(ctx).getOrElse(Map.empty[String, HttpContent])))) Pass() else Reject() }
  }
}
