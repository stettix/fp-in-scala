package net.janvsmachine.fpinscala

import scala.annotation.tailrec

sealed trait List[+A] {
  def tail: List[A]
}
case object Nil extends List[Nothing] {
  def tail = throw new IllegalArgumentException("Can't get tail of empty list")
}
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

object Chapter3 extends App {

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil        ⇒ throw new IllegalArgumentException("Can't get tail of empty list")
    case Cons(h, t) ⇒ t
  }

  // Exercise 3.3
  def setHead[A](as: List[A], newHead: A): List[A] = as match {
    case Nil        ⇒ throw new IllegalArgumentException("Can't replace head of empty list")
    case Cons(h, t) ⇒ Cons(newHead, t)
  }

  // Exercise 3.4
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil         ⇒ Nil
    case l if n == 0 ⇒ l
    case Cons(h, t)  ⇒ drop(t, n - 1)
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

  // Exercise 3.8.
  val l = List(1, 2, 3, 4, 5, 6)
  assert(foldRight(l, Nil.asInstanceOf[List[Int]])(Cons(_, _)) == l,
    "Using Nil and Cons as args to foldLeft should make copy of list")

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = {
    @tailrec
    def foldLeftAcc[A, B](as: List[A], acc: B)(f: (B, A) ⇒ B): B = as match {
      case Nil        ⇒ acc
      case Cons(h, t) ⇒ foldLeftAcc(t, f(acc, h))(f)
    }
    foldLeftAcc(as, z)(f)
  }

  // Exercise 3.11
  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  assert(sum(l) == 21)

  def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
  assert(product(l) == 2 * 3 * 4 * 5 * 6)

  // Exercise 3.12.
  def reverseFold[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((xs: List[A], x: A) ⇒ Cons(x, xs))

  assert(reverseFold(l) == List(6, 5, 4, 3, 2, 1))

  // Exercise 3.13: Write foldLeft in terms of foldRight!
  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B =
    foldRight(as, z)((a: A, b: B) ⇒ f(b, a))

  assert(foldLeft(l, 0)(_ + _) == foldLeftR(l, 0)(_ + _))

  // foldRight in terms of foldLeft!
  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(as, z)((b: B, a: A) ⇒ f(a, b))

  assert(foldRight(l, 0)(_ + _) == foldRightL(l, 0)(_ + _))

  // Exercise 3.14
  def append[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)((a: A, as: List[A]) ⇒ Cons(a, as))

  val l2 = List(7, 8, 9)
  assert(append(l, l2) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))

}
