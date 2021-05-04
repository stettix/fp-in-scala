package net.janvsmachine.fpinscala

import scala.annotation.tailrec

object Chapter2 {

  // Exercise 2.2.

  // A compact version, using some of the niceties in the standard Scala library.
  def isSortedC[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val pairs = as.view.zip(as drop 1)
    pairs.forall(p => p._1 == p._2 || ordered(p._1, p._2))
  }

  // A more verbose but fundamental version.
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (as.length - n < 2)
        true
      else (as(n) == as(n + 1) || ordered(as(n), as(n + 1))) && loop(n + 1)
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 2.3.
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4.
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5.
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
