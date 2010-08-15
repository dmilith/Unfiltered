package unfiltered.server

import org.specs._

object PlanSpec extends Specification with unfiltered.spec.Served {
  import unfiltered.response._
  import unfiltered.request._
  import unfiltered.request.{Path => UFPath}
  
  import dispatch._
  
  def setup = { _.filter(unfiltered.Planify {
      case GET(UFPath("/filter", _)) => {
        println("should pass to next filter")
        Pass
      }
    }).filter(unfiltered.Planify {
      case GET(UFPath("/filter", _)) => ResponseString("test") ~> Ok
    }).context("/filter2") {
      _.filter(unfiltered.Planify {
        case GET(UFPath("/test2", _)) => ResponseString("test2") ~> Ok
      })
    }
  }
  
  "A Plan" should {
    "filter on the same context path" in {
      Http(host / "filter" as_str)  must_=="test"
    }
    "filter on a second context path with overlapping name" in {
      Http(host / "filter2" / "test2" as_str)  must_=="test2"
    }
  }
}
