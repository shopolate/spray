package cc.spray.http

case class ContentDisposition(disposition : String, parameters : Map[String, String])
{
  def value: String = disposition + parameters.map(p => "; %s=\"%s\"".format(p._1, p._2)).mkString

  override def equals(obj: Any) = obj match {
    case x: ContentDisposition => disposition == x.disposition && parameters.toArray.sorted.sameElements(x.parameters.toArray.sorted)
    case _ => false
  }
}