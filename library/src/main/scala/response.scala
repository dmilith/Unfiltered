package unfiltered.response

import Unfiltered.Handler

import java.io.PrintWriter
import javax.servlet.http.HttpServletResponse

object Pass extends Handler {
  def apply(res: HttpServletResponse) = res
}

trait Responder extends Handler {
  def apply(res: HttpServletResponse) = { 
    respond(res)
    res
  }
  def respond(res: HttpServletResponse)
  def ~> (that: Handler) = new ResponderF(this andThen that)
}
class ResponderF(f: Handler) extends Responder {
  def respond(res: HttpServletResponse) = f(res)
}

case class Redirect(loc: String) extends Responder {
  def respond(res: HttpServletResponse) { res.sendRedirect(loc) }
}

case class Status(code: Int) extends Responder {
  def respond(res: HttpServletResponse) { res.setStatus(code) }
}
object NotModified extends Status(HttpServletResponse.SC_NOT_MODIFIED)
object NotFound extends Status(HttpServletResponse.SC_NOT_FOUND)

case class ContentType(content_type: String) extends Responder {
  def respond(res: HttpServletResponse) {
    res.setContentType("%s; charset=%s".format(content_type, charset))
  }
  def charset = "utf-8"
}
object CssContent extends ContentType("text/css")
object HtmlContent extends ContentType("text/html")

trait ResponseWriter extends Responder {
  def respond(res: HttpServletResponse) {
    val writer = res.getWriter()
    try { write(writer) }
    finally { writer.close() }
  }
  def write(writer: PrintWriter): Unit
}
case class ResponseString(content: String) extends ResponseWriter {
  def write(writer: PrintWriter) { writer.write(content) }
}

case class Html(nodes: scala.xml.NodeSeq) extends ResponderF(HtmlContent ~> ResponseString(nodes.toString))
