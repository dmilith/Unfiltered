package unfiltered.response

import org.specs._

object PassAndThenSpec extends Specification with unfiltered.spec.Served {
  import unfiltered.response._
  import unfiltered.request._
  import unfiltered.request.{Path => UFPath}
  
  import dispatch._
  
  class Around extends unfiltered.Planify({
    case GET(UFPath("/h", _)) => PassAndThen after {
      case _ => new HeaderName("x-test")("passed")
    }
    case GET(UFPath("/b", _)) => PassAndThen after {
      case _ => ResponseString("""[{"msg":"howdy partner"}]""")
    }
  })
  
  class TestPlan extends unfiltered.Planify({
    case GET(UFPath("/h", _)) => JsonContent ~> ResponseString("""[{"msg":"howdy partner"}]""")
    case GET(UFPath("/b", _)) => JsonContent
  })
  
  def setup = { _.filter(new Around).filter(new TestPlan) }
  
  "PassAndThen" should {
    "Pass and then execute some function setting a body" in {
      val (body, headers) = Http(host / "b" >+ { r => (r as_str, r >:> { h => h }) })
      println(headers)
      headers must havePair(("Content-Type" -> Set("application/json; charset=utf-8")))
      body must_=="""[{"msg":"howdy partner"}]"""
    }
    /* the following test fails, the response after the `later` fn is called contains the header
       but the response that dispatch recieves does not contain it
    "Pass and then execute some function appending a header" in {
      val (body, headers) = Http(host / "h" >+ { r => (r as_str, r >:> { h => h }) })
      println(headers)
      headers must havePair(("x-test" -> Set("passed")))
      headers must havePair(("Content-Type" -> Set("application/json; charset=utf-8")))
      body must_=="""[{"msg":"howdy partner"}]"""
    }
    */
  }
}
