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

import http._
import HttpHeaders._
import HttpMethods._
import MediaTypes._
import HttpCharsets._
import test.AbstractSprayTest
import marshalling.{UnmarshallerBase, MarshallerBase}
import xml.{XML, NodeSeq}

class MarshallingDirectivesSpec extends AbstractSprayTest {

  case class CustomContent(text : String)

  implicit object CustomContentUnmarshaller extends UnmarshallerBase[CustomContent] {
    val canUnmarshalFrom = ContentTypeRange(`text/xml`, `ISO-8859-2`) ::
                           ContentTypeRange(`text/html`) ::
                           ContentTypeRange(`application/xhtml+xml`) :: Nil

    def unmarshal(content: HttpContent) = protect { CustomContent(XML.load(content.inputStream).text) }
  }
  
  implicit object CustomContentMarshaller extends MarshallerBase[CustomContent] {
    val canMarshalTo = ContentType(`application/xhtml+xml`) :: ContentType(`text/xml`, `UTF-8`) :: Nil
    def marshal(value: CustomContent, contentType: ContentType) = NodeSeqMarshaller.marshal(<custom-content>{value.text}</custom-content>, contentType)
  }
  
  "The 'contentAs' directive" should {
    "extract an object from the requests HttpContent using the in-scope Unmarshaller" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`text/xml`), "<p>cool</p>")))) {
        content(as[NodeSeq]) { xml => _.complete(xml) }
      }.response.content.as[NodeSeq] mustEqual Right(<p>cool</p>) 
    }
    "return a RequestEntityExpectedRejection rejection if the request has no entity" in {
      test(HttpRequest(PUT)) {
        content(as[NodeSeq]) { _ => completeOk }
      }.rejections mustEqual Set(RequestEntityExpectedRejection)
    }
    "return an UnsupportedRequestContentTypeRejection if no matching unmarshaller is in scope" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`text/css`), "<p>cool</p>")))) {
        content(as[NodeSeq]) { _ => completeOk }
      }.rejections mustEqual Set(UnsupportedRequestContentTypeRejection(NodeSeqUnmarshaller.canUnmarshalFrom))
    }
  }
  
  "The 'optionalContentAs' directive" should {
    "extract an object from the requests HttpContent using the in-scope Unmarshaller" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`text/xml`), "<p>cool</p>")))) {
        optionalContent(as[NodeSeq]) { optXml => _.complete(optXml.get) }
      }.response.content.as[NodeSeq] mustEqual Right(<p>cool</p>) 
    }
    "extract None if the request has no entity" in {
      test(HttpRequest(PUT)) {
        optionalContent(as[NodeSeq]) { echoComplete }
      }.response.content.as[String] mustEqual Right("None")
    }
    "return an UnsupportedRequestContentTypeRejection if no matching unmarshaller is in scope" in {
      test(HttpRequest(PUT, content = Some(HttpContent(ContentType(`text/css`), "<p>cool</p>")))) {
        optionalContent(as[NodeSeq]) { _ => completeOk }
      }.rejections mustEqual Set(UnsupportedRequestContentTypeRejection(NodeSeqUnmarshaller.canUnmarshalFrom))
    }
  }
  
  "The 'produce' directive" should {
    "provide a completion function converting custom objects to HttpContent using the in-scope marshaller" in {
      test(HttpRequest(GET)) {
        produce(instanceOf[CustomContent]) { prod =>
          _ => prod(CustomContent("abc"))
        }
      }.response.content mustEqual Some(HttpContent(ContentType(`application/xhtml+xml`, `ISO-8859-1`), "<custom-content>abc</custom-content>"))
    }
    "return a UnacceptedResponseContentTypeRejection rejection if no acceptable marshaller is in scope" in {
      test(HttpRequest(GET, headers = List(`Accept`(`text/css`)))) {
        produce(instanceOf[CustomContent]) { prod =>
          _ => prod(CustomContent("abc"))
        }
      }.rejections mustEqual Set(UnacceptedResponseContentTypeRejection(CustomContentMarshaller.canMarshalTo))
    }
    "convert the response content to an accepted charset" in {
      test(HttpRequest(GET, headers = List(`Accept-Charset`(`UTF-8`)))) {
        produce(instanceOf[String]) { prod =>
          _ => prod("Hällö")
        }
      }.response.content mustEqual Some(HttpContent(ContentType(`text/plain`, `UTF-8`), "Hällö"))
    }
  }
  
  "The 'handleWith' directive" should {
    def times2(x: CustomContent) = CustomContent(x.text + x.text)
    "support proper round-trip content unmarshalling/marshalling to and from a function" in {
      test(HttpRequest(PUT, headers = List(Accept(`text/xml`)),
        content = Some(HttpContent(ContentType(`text/html`), "<custom-content>abc</custom-content>")))) {
        handleWith(times2)
      }.response.content mustEqual Some(HttpContent(ContentType(`text/xml`, `UTF-8`), "<custom-content>abcabc</custom-content>"))
    }
    "result in UnsupportedRequestContentTypeRejection rejection if there is no unmarshaller supporting the requests charset" in {
      test(HttpRequest(PUT, headers = List(Accept(`text/xml`)),
        content = Some(HttpContent(ContentType(`text/xml`, `UTF-8`), "<custom-content>abc</custom-content>")))) {
        handleWith(times2)
      }.rejections mustEqual Set(UnsupportedRequestContentTypeRejection(CustomContentUnmarshaller.canUnmarshalFrom))
    }
    "result in an UnacceptedResponseContentTypeRejection rejection if there is no marshaller supporting the requests Accept-Charset header" in {
      test(HttpRequest(PUT, headers = List(Accept(`text/xml`), `Accept-Charset`(`UTF-16`)),
        content = Some(HttpContent(ContentType(`text/html`), "<custom-content>abc</custom-content>")))) {
        handleWith(times2)
      }.rejections mustEqual Set(UnacceptedResponseContentTypeRejection(CustomContentMarshaller.canMarshalTo))
    }
  }
  
}