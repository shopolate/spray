package cc.spray
package test

import http._
import org.specs2.mutable._

abstract class AbstractSprayTest extends Specification with SprayTest with Directives with DontDetach {

  val Ok = HttpResponse(StatusCodes.OK)
  val completeOk: Route = { _.complete(Ok) }
  
}