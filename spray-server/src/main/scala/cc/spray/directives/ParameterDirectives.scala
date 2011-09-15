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

package cc.spray
package directives

import http.HttpContent
import marshalling.{DefaultUnmarshallers, CantUnmarshal, UnmarshalWith}

private[spray] trait ParameterDirectives {
  this: BasicDirectives =>

  private type FM[A] = FieldMatcher[A]
  
  /**
   * Returns a Route that rejects the request if a query parameter with the given name cannot be found.
   * If it can be found the parameters value is extracted and passed as argument to the inner Route building function. 
   */
  def parameter[A](pm: FM[A]): SprayRoute1[A] = filter1[A] { ctx =>
    pm(extractQueryParamsContent(ctx)) match {
      case Right(value) => Pass.withTransform(value) {
        _.cancelRejections {
          _ match {
            case MissingQueryParamRejection(n) if n == pm.name => true
            case MalformedQueryParamRejection(_, Some(n)) if n == pm.name => true
            case _ => false
          }
        }
      }
      case Left(MissingField(x)) => Reject(MissingQueryParamRejection(x))
      case Left(MalformedField(error, param)) => Reject(MalformedQueryParamRejection(error, Some(param)))
      case Left(ContentIncompatibility(onlyFrom, _)) => Reject(UnsupportedRequestContentTypeRejection(onlyFrom)) // Not exactly the right rejection.
    }
  }

  private def extractQueryParamsContent(ctx : RequestContext) = ctx.request.queryParams.mapValues(HttpContent(_))


  /**
   * Returns a Route that rejects the request if a query parameter with the given name cannot be found.
   * If it can be found the parameters value is extracted and passed as argument to the inner Route building function.
   */
  def parameters[A](a: FM[A]): SprayRoute1[A] = parameter(a)

  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B](a: FM[A], b: FM[B]): SprayRoute2[A, B] = {
    parameter(a) & parameter(b)
  }  

  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C](a: FM[A], b: FM[B], c: FM[C]): SprayRoute3[A, B, C] = {
    parameters(a, b) & parameter(c)
  }

  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C, D](a: FM[A], b: FM[B], c: FM[C], d: FM[D]): SprayRoute4[A, B, C, D] = {
    parameters(a, b, c) & parameter(d)
  }

  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C, D, E](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E]): SprayRoute5[A, B, C, D, E] = {
    parameters(a, b, c, d) & parameter(e)
  }
  
  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C, D, E, F](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                   f: FM[F]): SprayRoute6[A, B, C, D, E, F] = {
    parameters(a, b, c, d, e) & parameter(f)
  }
  
  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C, D, E, F, G](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                      f: FM[F], g: FM[G]): SprayRoute7[A, B, C, D, E, F, G] = {
    parameters(a, b, c, d, e, f) & parameter(g)
  }

  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C, D, E, F, G, H](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                         f: FM[F], g: FM[G], h: FM[H]): SprayRoute8[A, B, C, D, E, F, G, H] = {
    parameters(a, b, c, d, e, f, g) & parameter(h)
  }

  /**
   * Returns a Route that rejects the request if the query parameters with the given names cannot be found.
   * If it can be found the parameter values are extracted and passed as arguments to the inner Route building function.
   */
  def parameters[A, B, C, D, E, F, G, H, I](a: FM[A], b: FM[B], c: FM[C], d: FM[D], e: FM[E],
                                            f: FM[F], g: FM[G], h: FM[H], i: FM[I]): SprayRoute9[A, B, C, D, E, F, G, H, I] = {
    parameters(a, b, c, d, e, f, g, h) & parameter(i)
  }
  
  /**
   * Returns a Route that rejects the request if the query parameter with the given name cannot be found or does not
   * have the required value.
   */
  def parameter(p: RequiredFieldMatcher) = filter { ctx => if (p(extractQueryParamsContent(ctx))) Pass() else Reject() }

  /**
   * Returns a Route that rejects the request if the query parameter with the given name cannot be found or does not
   * have the required value.
   */
  def parameters(p: RequiredFieldMatcher, more: RequiredFieldMatcher*) = {
    val allRPM = p +: more
    filter { ctx => if (allRPM.forall(_(extractQueryParamsContent(ctx)))) Pass() else Reject() }
  }

  implicit def fromSymbol(name: Symbol) = fromString(name.name)  
  
  implicit def fromString(name: String) = new SimpleFieldMatcher(name, DefaultUnmarshallers.StringUnmarshaller)
}

