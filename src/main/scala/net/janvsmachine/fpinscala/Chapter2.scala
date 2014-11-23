package net.janvsmachine.fpinscala

object Chapter2 {

  // Exercise 2.2.
  def isSorted[A](as: Array[A], ordered: (A, A) ⇒ Boolean): Boolean = {
    val pairs = as.zip(as drop 1)
    pairs.forall(p ⇒ p._1 == p._2 || ordered(p._1, p._2))
  }

  def partial1[A, B, C](a: A, f: (A, B) ⇒ C): B ⇒ C =
    (b: B) ⇒ f(a, b)

  // Exercise 2.3.
  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ (B ⇒ C) =
    (a: A) ⇒ (b: B) ⇒ f(a, b)

  // Exercise 2.4.
  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C =
    (a: A, b: B) ⇒ f(a)(b)

  // Exercise 2.5.
  def compose[A, B, C](f: B => C, g: A => B): A => C = 
    (a: A) => f(g(a))
}
