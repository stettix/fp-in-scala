package net.janvsmachine.fpinscala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             ⇒ 1
    case Branch(left, right) ⇒ 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value)         ⇒ value
    case Branch(left, right) ⇒ maximum(left) max maximum(right)
  }

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)             ⇒ 1
    case Branch(left, right) ⇒ 1 + (depth(left) max depth(right))
  }

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(v)             ⇒ Leaf(f(v))
    case Branch(left, right) ⇒ Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29
  def fold[A, B](t: Tree[A], z: B)(f: (B, A) ⇒ B, comb: (B, B) ⇒ B): B = t match {
    case Leaf(v)             ⇒ f(z, v)
    case Branch(left, right) ⇒ comb(fold(left, z)(f, comb), fold(right, z)(f, comb)) // fold(left, fold(right, z)(f))(f)
  }

  // TODO!
  //  def fold[A, B](t: Tree[A], z: B)(f: (B, A) ⇒ B, g: (B, B) ⇒ B): B = t match {
  //    case Leaf(v)             ⇒ g(z, f(z, v))
  //    case Branch(left, right) ⇒ ??? //g(fold(left, f(z + 1)(f, g), fold(right, z + 1)(f, g))
  //  }

  def depthF[A](t: Tree[A]): Int = fold(t, 0)((acc: Int, _: A) ⇒ acc + 1, (x: Int, y: Int) ⇒ x max y)

  def sizeF[A](t: Tree[A]): Int = fold(t, 0)((acc: Int, _: A) ⇒ acc + 1, (x: Int, y: Int) ⇒ x + y)

  def maximumF(t: Tree[Int]): Int = fold(t, Int.MinValue)((acc: Int, a: Int) ⇒ acc max a, (x: Int, y: Int) ⇒ x + y)

  // Could perhaps try to do the version of fold that works for size/maximum/depth?
  // And then see how I could adapt it to the map case too?
  //def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a: A => Leaf(f(a)), (x: B, y: B) => Branch(x, y)

}
