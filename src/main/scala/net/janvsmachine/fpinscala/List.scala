package net.janvsmachine.fpinscala

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil        ⇒ throw new IllegalArgumentException("Can't get tail of empty list")
    case Cons(_, t) ⇒ t
  }

  // Exercise 3.3
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil        ⇒ throw new IllegalArgumentException("Can't replace head of empty list")
    case Cons(_, t) ⇒ Cons(newHead, t)
  }

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil         ⇒ Nil
    case l if n <= 0 ⇒ l
    case Cons(_, t)  ⇒ drop(t, n - 1)
  }

  // Exercise 3.5
  def dropWhile[A](as: List[A], p: A ⇒ Boolean): List[A] = as match {
    case Cons(h, t) if p(h) ⇒ dropWhile(t, p)
    case _                  ⇒ as
  }

  // Exercise 3.6
  def dropLast[A](as: List[A]): List[A] = as match {
    case Nil          ⇒ Nil
    case Cons(h, Nil) ⇒ Nil
    case Cons(h, t)   ⇒ Cons(h, dropLast(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = as match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ f(h, foldRight(t, z)(f))
  }

  // Exercise 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = as match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)

  // Exercise 3.12.
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) ⇒ Cons(h, acc))

  // Exercise 3.13: Write foldLeft in terms of foldRight!
  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B =
    foldRight(as, z)((a: A, b: B) ⇒ f(b, a))

  // foldRight in terms of foldLeft!
  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(as, z)((b: B, a: A) ⇒ f(a, b))

  // Exercise 3.14
  def append[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)((a: A, as: List[A]) ⇒ Cons(a, as))

  // Exercise 3.16
  def add1(as: List[Int]): List[Int] = as match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x + 1, add1(xs))
  }

  def doublesToString(ds: List[Double]): List[String] = ds match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x.toString, doublesToString(xs))
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A ⇒ B): List[B] = as match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ Cons(f(x), map(xs)(f))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A ⇒ Boolean): List[A] = as match {
    case Nil                 ⇒ Nil
    case Cons(x, xs) if f(x) ⇒ Cons(x, filter(xs)(f))
    case Cons(x, xs)         ⇒ filter(xs)(f)
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A ⇒ List[B]): List[B] = as match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ append(f(x), flatMap(xs)(f))
  }

  // Exercise 3.21
  def filterFl[A](as: List[A])(p: A ⇒ Boolean): List[A] =
    flatMap(as)((a: A) ⇒ if (p(a)) List(a) else Nil)

  // Exercise 3.23
  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) ⇒ A): List[A] = (xs, ys) match {
    case (Cons(a, as), Cons(b, bs)) ⇒ Cons(f(a, b), zipWith(as, bs)(f))
    case _                          ⇒ Nil
  }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil)                   ⇒ true
    case (Nil, _)                   ⇒ false
    case (Cons(x, xs), Cons(y, ys)) ⇒ (x == y && hasSubsequence(xs, ys)) || hasSubsequence(xs, sub)
  }

  def length(as: List[Int]) = foldLeft(as, 0)((acc, _) ⇒ acc + 1)

}
