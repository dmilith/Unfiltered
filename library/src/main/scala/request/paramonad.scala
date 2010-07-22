package unfiltered.request

import unfiltered.response.Responder
import javax.servlet.http.HttpServletResponse

trait PotentialResponder

trait Parameter[A] extends PotentialResponder {
  def flatMap(f: A => CompleteResponder): CompleteResponder
  def map[B <: Responder](f: A => Responder): CompleteResponder
  def orFail(f: => Responder): IfMissing[Responder]
}

class FoundParameter[A](a: A) extends Parameter[A] {
  def flatMap(f: A => CompleteResponder) = f(a)
  def map[B <: Responder](f: A => B) = new FoundIfFound(a).map(f)
  def orFail(f: => Responder) = new FoundIfMissing(a)
}
class MissingParameter[A] extends Parameter[A] {
  def flatMap(f: A => CompleteResponder) = new MissingIfFound
  def map(f: A => Responder) = new MissingIfFound
  def orFail(f: => Responder) = new MissingIfMissing(f)
}

trait IfFound[A] extends PotentialResponder {
  def flatMap(f: A => CompleteResponder): CompleteResponder
  def map[B <: Responder](f: A => Responder): CompleteResponder
  def orFail[B <: Responder](f: => B): CompleteResponder
}
class FoundIfFound[A <: Responder](a: A) extends IfFound[A] {
  def flatMap(f: A => CompleteResponder) = f(a)
  def map(f: A => Responder) = new FoundIfFound(f(a))
  def orFail(f: => Responder) = new CompleteResponder(a)
}
class MissingIfFound extends IfFound[Responder] {
  def flatMap(f: A => CompleteResponder) = new MissingParameter[B]
  def map[B <: Responder](f: A => B) = new MissingIfFound[Responder]
  def orFail[B <: Responder](f: => B) = new CompleteResponder(f)
}

trait IfMissing[A] extends PotentialResponder[A]
class FoundIfMissing[A](a: A) extends IfMissing[A] {
  def flatMap[M[B] <: PotentialResponder[B], B <: Responder](f: A => M[B]) = f(a)
  def map[B <: Responder](f: A => B) = new CompleteResponder(f(a))
}
class MissingIfMissing[A <: Responder](a: A) extends IfMissing[A] {
  def flatMap[M[B] <: PotentialResponder[B], B <: Responder](f: A => M[B]) = f(a)
  def map[B <: Responder](f: A => B) = new CompleteResponder(a)
}

class CompleteResponder(delegate: Responder) extends PotentialResponder with Responder {
  def respond(req: HttpServletResponse) = delegate.respond(req)
  def flatMap[M[B] <: PotentialResponder[B], B <: Responder](f: A => M[B]) = f(delegate)
  def map[B <: Responder](f: A => B) = new CompleteResponder(f(delegate))
}