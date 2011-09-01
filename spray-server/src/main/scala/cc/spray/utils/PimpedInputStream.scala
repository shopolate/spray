package cc.spray.utils

import java.io.InputStream

class PimpedInputStream(underlying : InputStream)
{
  private  val BUF_SIZE = 4096

  /**
   * Reads the totality of an input stream for which we don't know the total length in advance.
   */
  def readAll : List[Byte] =
  {
    var res = List.empty[Byte]
    val buffer = new Array[Byte](BUF_SIZE)
    var numRead = 0

    do
    {
      numRead = underlying.read(buffer, 0, BUF_SIZE)
      res = res ++ buffer.take(numRead)
    }
    while (BUF_SIZE == numRead)

    res
  }
}