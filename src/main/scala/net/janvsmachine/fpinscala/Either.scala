package net.janvsmachine.fpinscala

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  def map[B](f: Nothing => B) = this
  def flatMap[EE >: E, B](f: Nothing => Either[EE, B]) = this
  def orElse[EE >: E, B](b: => Either[EE, B]) = b
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  def map[B](f: A => B) = Right(f(value))
  def flatMap[EE >: Nothing, B](f: A => Either[EE, B]) = f(value)
  def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]) = this
  def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C) = b.map(f(value, _))
}

object Either {
  def Try[A](a: => A): Either[Exception, A] = try Right(a) catch { case e: Exception => Left(e) }
}

object Examples {
  import Either._

  def insuranceRateQuote(age: Int, numTickets: Int) = 100 - age + 100 * numTickets

  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)

}
