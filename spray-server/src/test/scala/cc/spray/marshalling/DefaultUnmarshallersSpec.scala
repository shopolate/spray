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
package marshalling

import http._
import MediaTypes._
import HttpCharsets._
import xml.NodeSeq
import test.AbstractSprayTest
import utils.{MultiPartFormData, FormContent}

class DefaultUnmarshallersSpec extends AbstractSprayTest {
  
  "The StringUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to Strings" in {
      test(HttpRequest(content = Some(HttpContent("Hällö")))) {
        content(as[String]) { echoComplete }
      }.response.content.as[String] mustEqual Right("Hällö")
    }
  }

  "The SymbolUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a symbol" in {
      test(HttpRequest(content = Some(HttpContent("someSymbol")))) {
        content(as[Symbol]) { echoComplete }
      }.response.content.as[String].right.get mustEqual "'someSymbol"
    }
  }

  "The IntUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Int" in {
      test(HttpRequest(content = Some(HttpContent("-12345")))) {
        content(as[Int]) { echoComplete }
      }.response.content.as[String] mustEqual Right("-12345")
    }
  }

  "The HexIntUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Int" in {
      test(HttpRequest(content = Some(HttpContent("1A")))) {
        content(DefaultUnmarshallers.HexIntUnmarshaller) { echoComplete }
      }.response.content.as[String] mustEqual Right("26")
    }
  }

  "The LongUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Long" in {
      test(HttpRequest(content = Some(HttpContent("-12345")))) {
        content(as[Int]) { echoComplete }
      }.response.content.as[String] mustEqual Right("-12345")
    }
  }

  "The HexLongUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Long" in {
      test(HttpRequest(content = Some(HttpContent("1A")))) {
        content(DefaultUnmarshallers.HexLongUnmarshaller) { echoComplete }
      }.response.content.as[String] mustEqual Right("26")
    }
  }

  "The DoubleUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Double" in {
      test(HttpRequest(content = Some(HttpContent("4500.23")))) {
        content(as[Double]) { echoComplete }
      }.response.content.as[String] mustEqual Right("4500.23")
    }
  }

  "The FloatUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Float" in {
      test(HttpRequest(content = Some(HttpContent("4500.23")))) {
        content(as[Float]) { echoComplete }
      }.response.content.as[String] mustEqual Right("4500.23")
    }
  }

  "The ShortUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Short" in {
      test(HttpRequest(content = Some(HttpContent("12345")))) {
        content(as[Short]) { echoComplete }
      }.response.content.as[String] mustEqual Right("12345")
    }
  }

  "The ByteUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Byte" in {
      test(HttpRequest(content = Some(HttpContent("-123")))) {
        content(as[Byte]) { echoComplete }
      }.response.content.as[String] mustEqual Right("-123")
    }
  }

  "The BooleanUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to a Boolean" in {
      test(HttpRequest(content = Some(HttpContent("false")))) {
        content(as[Boolean]) { echoComplete }
      }.response.content.as[String] mustEqual Right("false")
    }
  }

  "The CharArrayUnmarshaller" should {
    "decode `text/plain` content in ISO-8859-1 to char arrays" in {
      test(HttpRequest(content = Some(HttpContent("Hällö")))) {
        content(as[Array[Char]]) { charArray => _.complete(charArray) }
      }.response.content.as[String] mustEqual Right("Hällö")
    }
  }

  "The NodeSeqUnmarshaller" should {
    "decode `text/xml` content in ISO-8859-1 to NodeSeqs" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`text/xml`, `ISO-8859-1`), "<int>Hällö</int>")))) {
        content(as[NodeSeq]) { xml => _.complete(xml.text) }
      }.response.content.as[String] mustEqual Right("Hällö")
    }
  }

  "The FormContentUnmarshaller" should {
    "correctly unmarshal HTML form content with one element" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`, `UTF-8`),
        "secret=x%A4%2154")))) {
        content(as[FormContent]) { echoComplete }
      }.response.content.as[String] mustEqual Right("FormContent(Map(secret -> x?!54))")
    }
    "correctly unmarshal HTML form content with no element" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`, `UTF-8`),
        "")))) {
        content(as[FormContent]) { echoComplete }
      }.response.content.as[String] mustEqual Right("FormContent(Map())")
    }
    "correctly unmarshal HTML form content with three elements" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`, `ISO-8859-1`),
        "email=test%40there.com&password=&username=dirk")))) {
        content(as[FormContent]) { echoComplete }
      }.response.content.as[String] mustEqual
              Right("FormContent(Map(email -> test@there.com, password -> , username -> dirk))")
    }
    "reject illegal form content" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`application/x-www-form-urlencoded`, `ISO-8859-1`),
        "key=really=not_good")))) {
        content(as[FormContent]) { echoComplete }
      }.rejections mustEqual Set(MalformedRequestContentRejection("'key=really=not_good' is not a valid form content"))
    }
  }

  "The MultiPartFormDataUnmarshaller" should {
    "correctly unmarshal HTML form content with one element" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`multipart/form-data`, Some(`UTF-8`), Some("XYZABC")),
        "--XYZABC\ncontent-disposition: form-data; name=\"email\"\n\ntest@there.com\n--XYZABC--")))) {
        content(as[MultiPartFormData]) { multiPart => _.complete(multiPart("email").content.as[String].right.get) }
      }.response.content.as[String] mustEqual Right("test@there.com")
    }
    "correctly unmarshal HTML form content mixed with a file" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`multipart/form-data`, Some(`UTF-8`), Some("XYZABC")),
        "--XYZABC\ncontent-disposition: form-data; name=\"email\"\n\ntest@there.com\n--XYZABC\ncontent-disposition: form-data; name=\"userfile\"; filename=\"test.dat\"\nContent-Type: application/octet-stream\nContent-Transfer-Encoding: binary\n\nfilecontent\n--XYZABC--")))) {
        content(as[MultiPartFormData]) { multiPart => _.complete(multiPart("email").content.as[String].right.get + multiPart("userfile").content.as[String].right.get) }
      }.response.content.as[String] mustEqual Right("test@there.comfilecontent")
    }
    "reject illegal form content" in {
      test(HttpRequest(content = Some(HttpContent(ContentType(`multipart/form-data`, Some(`UTF-8`), Some("XYZABC")),
        "--noboundary--")))) {
        content(as[MultiPartFormData]) { echoComplete }
      }.rejections mustEqual Set(MalformedRequestContentRejection("Missing start boundary"))
    }
  }
}