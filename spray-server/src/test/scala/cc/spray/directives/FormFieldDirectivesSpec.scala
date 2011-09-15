package cc.spray
package directives

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

import http._
import HttpMethods._
import MediaTypes._
import HttpCharsets._
import test.AbstractSprayTest

class FormFieldDirectivesSpec extends AbstractSprayTest {

  "The 'formField' extraction directive" should {
    "extract the value of given required 'application/x-www-form-urlencoded' form field" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "name=Parsons&FirstName=Ellen")))) {
        formFields("name", 'FirstName) { (name, firstName) =>
          _.complete(firstName + name)
        }
      }.response.content.as[String] mustEqual Right("EllenParsons")
    }
    "extract the value of given required 'multipart/form-data' form field" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`multipart/form-data`, Some(`UTF-8`), Some("XYZABC")), "--XYZABC\ncontent-disposition: form-data; name=\"name\"\n\nParsons\n--XYZABC\ncontent-disposition: form-data; name=\"FirstName\"\n\nEllen\n--XYZABC--")))) {
        formFields("name", 'FirstName) { (name, firstName) =>
          _.complete(firstName + name)
        }
      }.response.content.as[String] mustEqual Right("EllenParsons")
    }
    "ignore additional form fields" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "name=Parsons&FirstName=Ellen&age=29")))) {
        formFields("name", 'FirstName) { (name, firstName) =>
          _.complete(firstName + name)
        }
      }.response.content.as[String] mustEqual Right("EllenParsons")
    }
    "reject the request with a MalformedRequestContentRejection if a required form field is missing" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "name=Parsons&sex=female")))) {
        formFields('name, 'FirstName, 'age) { (name, firstName, age) =>
          completeOk
        }
      }.rejections mustEqual Set(MissingFormFieldRejection("FirstName"))
    }
    "reject the request with a MissingFormFieldRejection if the request content is not a form" in {
      test(HttpRequest(PUT, content = Some(HttpContent("name=Parsons&sex=female")))) {
        formFields('name, 'FirstName, 'age) { (name, firstName, age) =>
          completeOk
        }
      }.rejections mustEqual Set(MalformedRequestContentRejection("The request content does not contain a form of type 'application/x-www-form-urlencoded' nor 'multipart/form-data'."))
    }
    "supply the default value if an optional form field is missing" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "name=Parsons&FirstName=Ellen")))) {
        formFields("name"?, 'FirstName, 'age ? "29", 'eyes?) { (name, firstName, age, eyes) =>
          _.complete(firstName + name + age + eyes)
        }
      }.response.content.as[String] mustEqual Right("EllenSome(Parsons)29None")
    }
  }
  
  "The 'formField' requirement directive" should {
    "block requests that do not contain the required formField" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "age=19")))) {
        formField('nose ! "large") { completeOk }
      }.handled must beFalse
    }
    "block requests that contain the required formField but with an unmatching value" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "age=19&nose=small")))) {
        formField('nose ! "large") { completeOk }
      }.handled must beFalse
    }
    "let requests pass that contain the required form field with its required value" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "nose=large&eyes=blue")))) {
        formField('nose ! "large") { completeOk }
      }.response mustEqual Ok
    }
    "be useable for method tunneling" in {
      val route = {
        path("person") {
          (post | formField('method ! "post")) {
            _.complete("POST")
          } ~
          get { _.complete("GET") }
        }
      }
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "method=post")), uri = "/person")) {
        route 
      }.response.content.as[String] mustEqual Right("POST")
      test(HttpRequest(POST, content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`), "")), uri = "/person")) {
        route 
      }.response.content.as[String] mustEqual Right("POST")
      test(HttpRequest(uri = "/person")) {
        route 
      }.response.content.as[String] mustEqual Right("GET")
    }
  }

}