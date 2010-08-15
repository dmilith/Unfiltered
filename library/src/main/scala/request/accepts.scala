package unfiltered.request

/** Accepts request header extractor */
object Accepts {
  
  trait Accepting {
    val contentType: String

    val ext: String

    def unapply(r: javax.servlet.http.HttpServletRequest) = {
      val pathSuffix = r.getRequestURI.substring(r.getContextPath.length).split("[.]").lastOption
      r match {
        case Accept(values, _) =>
          if(!values.filter(_.equalsIgnoreCase(contentType)).isEmpty) Some(r)
          else if(!values.filter(_=="*/*").isEmpty && ext == pathSuffix.getOrElse("not")) Some(r)
          else None
        case _ => pathSuffix match {
          case Some(pathSuffix) if(pathSuffix == ext) => Some(r)
          case _ => None
        }
      }
    }
  }

  object Json extends Accepting {
    val contentType = "application/json"
    val ext = "json"
  }

  object Xml extends Accepting {
    val contentType = "text/xml"
    val ext = "xml"
  }
  
  object Html extends Accepting {
    val contentType = "text/html"
    val ext = "html"
  }
  
  object Csv extends Accepting {
    val contentType = "text/csv"
    val ext = "csv"
  }
}
