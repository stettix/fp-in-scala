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

object Option {

  import Chapter3._

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = xs match {
    case Seq() ⇒ None
    case values ⇒ {
      val m = xs.sum / xs.length
      val divs = xs.foldLeft(0.0d) { case (acc, x) ⇒ acc + Math.pow(x - m, 2) }
      Some(divs / xs.length)
    }
  }

  // Exercise 4.3
  def map2a[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) ⇒ Some(f(x, y))
    case _                  ⇒ None
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] =
    a.flatMap(x ⇒ b.map(y ⇒ f(x, y)))

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    foldRight[Option[A], Option[List[A]]](xs, Some(Nil))((x, acc) ⇒ (x, acc) match {
      case (Some(v), Some(acc)) ⇒ Some(Cons(v, acc))
      case _                    ⇒ None
    })

}
