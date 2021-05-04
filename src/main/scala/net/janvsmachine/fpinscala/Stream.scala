package net.janvsmachine.fpinscala

import Stream._

sealed trait Stream[+A] {
  def toList: List[A]
  def headOption: Option[A]

  def exists(p: A => Boolean): Boolean = this match {
    case SCons(h, t) => p(h()) || t().exists(p)
    case _           => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case SCons(h, t) => f(h(), t().foldRight(z)(f))
    case _           => z
  }

  def take(n: Int): Stream[A] = this match {
    case SCons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _                    => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case SCons(h, t) if n > 0 => t().drop(n - 1)
    case _                    => this
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, res) => p(a) || res)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, res) => p(a) && res)

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, res) => if (p(a)) cons(a, res.takeWhile(p)) else empty)

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, res) => cons(f(a), res))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, res) => if (p(a)) cons(a, res) else res)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, res) => cons(a, res))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, res) => f(a).append(res))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  // A bit odd - is this right?
  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case SCons(h, t) => Some((f(h()), t()))
    case _           => None
  }

}

case object Empty extends Stream[Nothing] {
  def toList: List[Nothing] = Nil
  def headOption: Option[Nothing] = None
}

case class SCons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  def toList: List[A] = Cons(h(), t().toList)
  def headOption: Option[A] = Some(h())
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]) = {
    lazy val head = hd
    lazy val tail = tl
    SCons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(start: Int): Stream[Int] = cons(start, from(start + 1))

  def fibs: Stream[Long] = cons(0, cons(1, fibs(0, 1)))
  private def fibs(prev1: Long, prev2: Long): Stream[Long] = {
    val next = prev1 + prev2
    cons(next, fibs(prev2, next))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((nextVal, nextState)) => cons(nextVal, unfold(nextState)(f))
    case None                       => Stream.empty
  }

  private def fibsNext(prev: (Long, Long)): Option[(Long, (Long, Long))] = {
    val nextVal = prev._1 + prev._2
    Some(nextVal, (prev._2, nextVal))
  }
  def ones = unfold(1)(_ => Some((1, 1)))
  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
  def from2(start: Int): Stream[Int] = unfold(start)((last: Int) => Some((last, last + 1)))
  def fibs2: Stream[Long] = cons(0, cons(1, unfold[Long, (Long, Long)]((0, 1))(fibsNext)))

}
