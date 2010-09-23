package unfiltered.response


case class Status(code: Int) extends Responder {
  def respond[T](res: HttpResponse[T]) { res.setStatus(code) }
}

// http://www.iana.org/assignments/http-status-codes & http://en.wikipedia.org/wiki/List_of_HTTP_status_codes

object Continue extends Status(100)
object SwitchingProtocols extends Status(101)
object Processing extends Status(102)

object Ok extends Status(200)
object Created extends Status(201)
object Accepted extends Status(202)
object NonAuthoritativeInformation extends Status(203)
object NoContent extends Status(204)
object ResetContent extends Status(205) 
object PartialContent extends Status(206)
object MultiStatus extends Status(207)
object AlreadyReported extends Status(208)
object IMUsed extends Status(226)

object MultipleChoices extends Status(300)
object MovedPermanently extends Status(301)
object Found extends Status(302) 
object SeeOther extends Status(303)
object NotModified extends Status(304)
object UseProxy extends Status(305)
object TemporaryRedirect extends Status(307)

object BadRequest extends Status(400)
object Unauthorized extends Status(401)
object PaymentRequired extends Status(402)
object Forbidden extends Status(403)
object NotFound extends Status(404)
object MethodNotAllowed extends Status(405)
object NotAcceptable extends Status(406)
object ProxyAuthenticationRequired extends Status(407)
object RequestTimeout extends Status(408)
object Conflict extends Status(409)
object Gone extends Status(410)
object LengthRequired extends Status(411)
object PreconditionFailed extends Status(412)
object RequestEntityTooLarge extends Status(413)
object RequestURITooLong extends Status(414)
object UnsupportedMediaType extends Status(415)
object RequestedRangeNotSatisfiable extends Status(416) 
object ExpectationFailed extends Status(417)
object TeaPot extends Status(418)
object TooManyConnections extends Status(421)
object UnprocessableEntity extends Status(422)
object Locked extends Status(423)
object FailedDependency extends Status(424)
object UnorderedCollection extends Status(425)
object UpdateRequired extends Status(426)

object InternalServerError extends Status(500)
object NotImplemented extends Status(501)
object BadGateway extends Status(502)
object ServiceUnavailable extends Status(503)
object GatewayTimeout extends Status(504)
object VersionNotSupported extends Status(505)
object VariantAlsoNegotiates extends Status(506)
object InsufficientStorage extends Status(507)
object LoopDetected extends Status(508)
object NotExtended extends Status(510)
