/*
 * Copyright (C) 2011 Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cc

import util.matching.Regex
import collection.immutable.LinearSeq
import spray.http._
import spray.marshalling._
import spray.utils._
import akka.dispatch.Future
import java.io.InputStream

package object spray {

  type Route = RequestContext => Unit
  type ContentTypeResolver = (String, Option[HttpCharset]) => ContentType
  type Marshaller[A] = (ContentType => Option[ContentType]) => Marshalling[A]
  type Unmarshaller[A] = ContentType => Unmarshalling[A]
  type RouteFilter[T <: Product] = RequestContext => FilterResult[T]
  type GeneralAuthenticator[U] = RequestContext => Either[Rejection, U]
  type UserPassAuthenticator[U] = Option[(String, String)] => Option[U]
  type CacheKeyer = RequestContext => Option[Any]
  type RequiredFieldMatcher = Map[String, String] => Boolean
  type FieldMap = Map[String, HttpContent]

  def make[A, U](a: A)(f: A => U): A = { f(a); a }
  
  def marshaller[T](implicit m: Marshaller[T]) = m
  def unmarshaller[T](implicit um: Unmarshaller[T]) = um
  def simpleParser[T](implicit sp: SimpleParser[T]) = sp

  private lazy val emptyPartial = new PartialFunction[Any, Any] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new IllegalStateException
  }

  def emptyPartialFunc[A, B] = emptyPartial.asInstanceOf[PartialFunction[A, B]]

  // implicits
  implicit def pimpLinearSeq[A](seq: LinearSeq[A]): PimpedLinearSeq[A] = new PimpedLinearSeq[A](seq)
  implicit def pimpClass[A](clazz: Class[A]): PimpedClass[A] = new PimpedClass[A](clazz)
  implicit def pimpProduct(product: Product): PimpedProduct = new PimpedProduct(product)
  implicit def pimpRegex(regex: Regex) = new PimpedRegex(regex)
  implicit def pimpFuture[F <: Future[_]](future: F) = new PimpedFuture(future)
  implicit def pimpString(s: String) = new PimpedString(s)
  implicit def pimpInputStream(stream: InputStream) = new PimpedInputStream(stream)
}