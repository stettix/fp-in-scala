package net.janvsmachine.fpinscala

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

object Chapter3 {

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
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if !p(h) => as
    case Cons(h, t) => dropWhile(t, p)
  }
  
  // Exercise 3.6
  def dropLast[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, dropLast(t))
  }  

}
