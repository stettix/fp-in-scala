package net.janvsmachine.fpinscala

//
// For this one, I've written the functions in the style where each operation
// is defined in a base trait and implemented in derived objects/classes.
// It's only a stylistic difference, though looing at it, I do find it neat avoiding
// the pattern match on the None vs Some cases in each method - and calling the method
// on values instead of passing values in as the first argument to each function seems neater too.
//
// Functions on multiple Options are still defined as free-standing functions, of course.
//

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: B): B
  def orElse[B >: A](default: B): Option[B]
  def filter(f: A => Boolean): Option[A]
}

// Exercise 4.1

case class Some[+A](value: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(value))
  def flatMap[B](f: A => Option[B]): Option[B] = f(value)
  def getOrElse[B >: A](default: B): B = value
  def orElse[B >: A](default: B): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if (f(value)) Some(value) else None
}

object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def getOrElse[B >: Nothing](default: B): B = default
  def orElse[B >: Nothing](default: B): Option[B] = Some(default)
  def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Option {

  import List._

  // From book.
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  // Exercise 4.3
  def map2a[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _                  => None
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x, y)))

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    foldRight[Option[A], Option[List[A]]](xs, Some(Nil))((x, acc) => (x, acc) match {
      case (Some(v), Some(acc)) => Some(Cons(v, acc))
      case _                    => None
    })

  def traverse[A, B](xs: List[Option[A]])(f: A => B): Option[List[B]] =
    foldRight[Option[A], Option[List[B]]](xs, Some(Nil))((x, acc) => (x, acc) match {
      case (Some(v), Some(acc)) => Some(Cons(f(v), acc))
      case _                    => None
    })

  def sequence2[A](xs: List[Option[A]]): Option[List[A]] = traverse(xs)(x => x)

}
