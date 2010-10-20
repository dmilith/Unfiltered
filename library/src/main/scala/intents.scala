package unfiltered

import unfiltered.request.HttpRequest
import unfiltered.response.{ResponseFunction,HttpResponse,Pass}

object Roundtrip {
  /** A rountrip intent is a set of instructions for producting
   * a complete response to a request. Plans that contain intents
   * of this type can be run against a general set of tests. */
  type Intent[A] = PartialFunction[HttpRequest[A], ResponseFunction]
}
