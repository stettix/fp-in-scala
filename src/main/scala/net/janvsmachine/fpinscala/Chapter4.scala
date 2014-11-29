package net.janvsmachine.fpinscala

trait Option[+A] {
  def map[B](f: A ⇒ B): Option[B]
  def flatMap[B](f: A ⇒ Option[B]): Option[B]
  def getOrElse[B >: A](default: B): B
  def orElse[B >: A](default: B): Option[B]
  def filter(f: A ⇒ Boolean): Option[A]
}

// Exercise 4.1

case class Some[+A](value: A) extends Option[A] {
  def map[B](f: A ⇒ B): Option[B] = Some(f(value))
  def flatMap[B](f: A ⇒ Option[B]): Option[B] = f(value)
  def getOrElse[B >: A](default: B): B = value
  def orElse[B >: A](default: B): Option[B] = Some(value)
  def filter(f: A ⇒ Boolean): Option[A] = if (f(value)) Some(value) else None
}

object None extends Option[Nothing] {
  def map[B](f: Nothing ⇒ B): Option[B] = None
  def flatMap[B](f: Nothing ⇒ Option[B]): Option[B] = None
  def getOrElse[B >: Nothing](default: B): B = default
  def orElse[B >: Nothing](default: B): Option[B] = Some(default)
  def filter(f: Nothing ⇒ Boolean): Option[Nothing] = None
}

object Chapter4 {

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = xs match {
    case Seq() ⇒ None
    case values ⇒ {
      val m = xs.sum / xs.length
      val divs = xs.foldLeft(0.0d) { case (acc, x) ⇒ acc + Math.pow(x - m, 2) }
      Some(divs / xs.length)
    }
  }

}
