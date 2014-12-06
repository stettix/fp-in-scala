package net.janvsmachine.fpinscala

import Stream._

sealed trait Stream[+A] {
  def toList: List[A]
  def headOption: Option[A]

  def exists(p: A ⇒ Boolean): Boolean = this match {
    case SCons(h, t) ⇒ p(h()) || t().exists(p)
    case _           ⇒ false
  }

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = this match {
    case SCons(h, t) ⇒ f(h(), t().foldRight(z)(f))
    case _           ⇒ z
  }

  def exists2(p: A ⇒ Boolean): Boolean = foldRight(false)((a, res) ⇒ p(a) || res)

  def forAll(p: A ⇒ Boolean): Boolean = foldRight(true)((a, res) ⇒ p(a) && res)

  def takeWhile(p: A ⇒ Boolean): Stream[A] = foldRight(empty[A])((a, res) ⇒ if (p(a)) cons(a, res.takeWhile(p)) else empty())

  def map[B](f: A ⇒ B): Stream[B] = this match {
    case SCons(h, t) ⇒ cons(f(h()), t().map(f))
    case _           ⇒ empty()
  }
}

case object Empty extends Stream[Nothing] {
  def toList: List[Nothing] = Nil
  def headOption: Option[Nothing] = None
}

case class SCons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A] {
  def toList: List[A] = Cons(h(), t().toList)
  def headOption: Option[A] = Some(h())
}

object Stream {
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]) = {
    lazy val head = hd
    lazy val tail = tl
    SCons(() ⇒ head, () ⇒ tail)
  }
  def empty[A](): Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
}
