package net.janvsmachine.fpinscala

object Chapter2 {

  // Exercise 2.2.
  def isSorted[A](as: Array[A], ordered: (A, A) ⇒ Boolean): Boolean = {
    val pairs = as.zip(as drop 1)
    pairs.forall(p ⇒ p._1 == p._2 || ordered(p._1, p._2))
  }

}
