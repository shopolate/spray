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

import collection.JavaConversions._
import http._
import MediaTypes._
import MediaRanges._
import HttpCharsets._
import xml.{XML, NodeSeq}
import java.nio.ByteBuffer
import java.net.URLDecoder
import org.jvnet.mimepull.MIMEMessage
import utils.{BodyPart, MultiPartFormData, FormContent}
import cc.spray._
import cc.spray.http.HttpHeaders._

trait DefaultUnmarshallers {
  
  implicit object StringUnmarshaller extends UnmarshallerBase[String] {
    val canUnmarshalFrom = ContentTypeRange(`*/*`) :: Nil // we can convert anything to a string

    def unmarshal(content: HttpContent) = {
      val charset = content.contentType.charset.getOrElse(`ISO-8859-1`)
      Right(new String(content.buffer, charset.nioCharset))
    }
  }

  trait StringValueUnmarshaller[A] extends UnmarshallerBase[A] {
    val canUnmarshalFrom = ContentTypeRange(`text/plain`) :: Nil // we can convert only from pure text
    def unmarshal(value : String): Either[Rejection, A]
    def unmarshal(content: HttpContent) = try {
      unmarshal(StringUnmarshaller.unmarshal(content).right.get)
    }
    catch {
      case e : Exception => Left(MalformedRequestContentRejection(e.getMessage))
    }
  }

  implicit object SymbolUnmarshaller extends StringValueUnmarshaller[Symbol] {
    def unmarshal(value : String) = Right(Symbol(value))
  }

  implicit object IntUnmarshaller extends StringValueUnmarshaller[Int] {
    def unmarshal(value : String) = {
      try {
        Right(value.toInt)
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid 32-bit integer value"))
      }
    }
  }

  object HexIntUnmarshaller extends StringValueUnmarshaller[Int] {
    def unmarshal(value : String) = {
      try {
        Right(Integer.parseInt(value, 16))
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid 32-bit hexadecimal integer value"))
      }
    }
  }

  implicit object LongUnmarshaller extends StringValueUnmarshaller[Long] {
    def unmarshal(value : String) = {
      try {
        Right(value.toLong)
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid 64-bit integer value"))
      }
    }
  }

  object HexLongUnmarshaller extends StringValueUnmarshaller[Long] {
    def unmarshal(value : String) = {
      try {
        Right(java.lang.Long.parseLong(value, 16))
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid 64-bit hexadecimal integer value"))
      }
    }
  }

  implicit object DoubleUnmarshaller extends StringValueUnmarshaller[Double] {
    def unmarshal(value : String) = {
      try {
        Right(value.toDouble)
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid floating point value"))
      }
    }
  }

  implicit object FloatUnmarshaller extends StringValueUnmarshaller[Float] {
    def unmarshal(value : String) = {
      try {
        Right(value.toFloat)
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid floating point value"))
      }
    }
  }

  implicit object ShortUnmarshaller extends StringValueUnmarshaller[Short] {
    def unmarshal(value : String) = {
      try {
        Right(value.toShort)
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid 16-bit integer value"))
      }
    }
  }

  implicit object ByteUnmarshaller extends StringValueUnmarshaller[Byte] {
    def unmarshal(value : String) = {
      try {
        Right(value.toByte)
      } catch {
        case _: NumberFormatException => Left(MalformedRequestContentRejection("'" + value + "' is not a valid 8-bit integer value"))
      }
    }
  }

  implicit object BooleanUnmarshaller extends StringValueUnmarshaller[Boolean] {
    def unmarshal(value : String) = value.toLowerCase match {
      case "true" | "yes" | "on" => Right(true)
      case "false" | "no" | "off" => Right(false)
      case x => Left(MalformedRequestContentRejection("'" + x + "' is not a valid Boolean value"))
    }
  }

  implicit object CharArrayUnmarshaller extends UnmarshallerBase[Array[Char]] {
    val canUnmarshalFrom = ContentTypeRange(`*/*`) :: Nil // we can convert anything to a char array

    def unmarshal(content: HttpContent) = {
      val nioCharset = content.contentType.charset.getOrElse(`ISO-8859-1`).nioCharset
      val byteBuffer = ByteBuffer.wrap(content.buffer)
      val charBuffer = nioCharset.decode(byteBuffer)
      Right(charBuffer.array())
    }
  }
  
  implicit object NodeSeqUnmarshaller extends UnmarshallerBase[NodeSeq] {
    val canUnmarshalFrom = ContentTypeRange(`text/xml`) ::
                           ContentTypeRange(`text/html`) ::
                           ContentTypeRange(`application/xhtml+xml`) :: Nil

    def unmarshal(content: HttpContent) = protect {
      if (content.contentType.charset.isDefined) {
        XML.loadString(StringUnmarshaller.unmarshal(content).right.get)
      } else {
        XML.load(content.inputStream)
      }
    }
  }

  implicit object FormContentUnmarshaller extends UnmarshallerBase[FormContent] {
    val canUnmarshalFrom = ContentTypeRange(`application/x-www-form-urlencoded`) :: Nil
  
    def unmarshal(content: HttpContent) = protect {
      FormContent {
        val data = DefaultUnmarshallers.StringUnmarshaller.unmarshal(content).right.get
        val charset = content.contentType.charset.getOrElse(`ISO-8859-1`).aliases.head
        URLDecoder.decode(data, charset).fastSplit('&').filter(_ != "").map {
          _.fastSplit('=') match {
            case key :: value :: Nil => (key, value)
            case _ => throw new IllegalArgumentException("'" + data + "' is not a valid form content")
          }
        } (collection.breakOut)
      }
    }
  }
  
  implicit object MultiPartFormDataUnmarshaller extends UnmarshallerBase[MultiPartFormData] {
    val canUnmarshalFrom = ContentTypeRange(`multipart/form-data`) :: Nil

    def unmarshal(content: HttpContent) = protect {
      val mimeMsg = new MIMEMessage(content.inputStream, content.contentType.boundary.getOrElse(""))
      val parts = mimeMsg.getAttachments.map { part =>
        val content = part.readOnce().readAll.toArray
        val contentType = HttpHeader("Content-Type", part.getContentType) match {
          case `Content-Type`(t) => t
          case _ => ContentType(`text/plain`) // RFC2388: each part has an optional "Content-Type", which defaults to text/plain
        }
        val name = part.getHeader("Content-Disposition") match {
          case buffer if buffer != null => HttpHeader("Content-Disposition", buffer.head) match {
            case `Content-Disposition`(ContentDisposition(_, params)) if params.contains("name") => Some(params("name"))
            case _ => None
          }
          case _ => None
        }
        BodyPart(name, HttpContent(contentType, content))
      }
      MultiPartFormData(parts.toList)
    }

    private def nullOption[A](obj: A): Option[A] = if (obj == null) None else Some(obj)
  }

  implicit def pimpHttpContentWithAs1(c: HttpContent): HttpContentExtractor = new HttpContentExtractor(Some(c))
  implicit def pimpHttpContentWithAs2(c: Option[HttpContent]): HttpContentExtractor = new HttpContentExtractor(c)
  
  class HttpContentExtractor(content: Option[HttpContent]) {
    def as[A](implicit unmarshaller: Unmarshaller[A]): Either[Rejection, A] = content match {
      case Some(httpContent) => unmarshaller(httpContent.contentType) match {
        case UnmarshalWith(converter) => converter(httpContent)
        case CantUnmarshal(onlyFrom) => Left(UnsupportedRequestContentTypeRejection(onlyFrom))
      }
      case None => Left(RequestEntityExpectedRejection)
    }
  }
  
}

object DefaultUnmarshallers extends DefaultUnmarshallers