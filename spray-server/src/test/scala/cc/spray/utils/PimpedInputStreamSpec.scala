package cc.spray
package utils

import org.specs2.mutable.Specification
import java.io.ByteArrayInputStream

class PimpedInputStreamSpec extends Specification
{
  "readAll" should {
    "Return Nil when input stream is empty" in {
      new ByteArrayInputStream(Array.empty[Byte]).readAll mustEqual Nil
    }
    "Read all the bytes of an input stream" in {
      val input = List.fill[Byte](4096)(1) ++ List(2.toByte, 2.toByte)

      new ByteArrayInputStream(input.toArray).readAll mustEqual input
    }
  }
}