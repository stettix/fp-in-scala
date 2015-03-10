package net.janvsmachine.fpinscala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size1[A](t: Tree[A]): Int = t match {
    case Leaf(_)             ⇒ 1
    case Branch(left, right) ⇒ 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def maximum1(t: Tree[Int]): Int = t match {
    case Leaf(value)         ⇒ value
    case Branch(left, right) ⇒ maximum1(left) max maximum1(right)
  }

  // Exercise 3.27
  def depth1[A](t: Tree[A]): Int = t match {
    case Leaf(_)             ⇒ 0
    case Branch(left, right) ⇒ 1 + (depth1(left) max depth1(right))
  }

  // Exercise 3.28
  def map1[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(v)             ⇒ Leaf(f(v))
    case Branch(left, right) ⇒ Branch(map1(left)(f), map1(right)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A])(f: A ⇒ B)(g: (B, B) ⇒ B): B = t match {
    case Leaf(v)             ⇒ f(v)
    case Branch(left, right) ⇒ g(fold(left)(f)(g), fold(right)(f)(g))
  }

  // Implementations in terms of fold.

  def depth[A](t: Tree[A]): Int = fold(t)(_ ⇒ 0)((x: Int, y: Int) ⇒ 1 + (x max y))

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)((x: Int, y: Int) ⇒ 1 + x + y)

  def maximum(t: Tree[Int]): Int = fold(t)(a => a)((x: Int, y: Int) ⇒ x max y)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}
