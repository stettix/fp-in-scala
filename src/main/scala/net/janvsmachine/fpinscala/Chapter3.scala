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

  // Exercise 3.16
  def add1(as: List[Int]): List[Int] = as match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x + 1, add1(xs))
  }
  assert(add1(l) == List(2, 3, 4, 5, 6, 7))

  def doublesToString(ds: List[Double]): List[String] = ds match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x.toString, doublesToString(xs))
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A ⇒ B): List[B] = as match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ Cons(f(x), map(xs)(f))
  }
  assert(map(l)(_ + 1) == List(2, 3, 4, 5, 6, 7))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A ⇒ Boolean): List[A] = as match {
    case Nil                 ⇒ Nil
    case Cons(x, xs) if f(x) ⇒ Cons(x, filter(xs)(f))
    case Cons(x, xs)         ⇒ filter(xs)(f)
  }
  assert(filter(l)(_ % 2 == 0) == List(2, 4, 6))

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A ⇒ List[B]): List[B] = as match {
    case Nil         ⇒ Nil
    case Cons(x, xs) ⇒ append(f(x), flatMap(xs)(f))
  }
  assert(flatMap(l)(x ⇒ List(x, x)) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))

  // Exercsise 3.21
  def filterFl[A](as: List[A])(p: A ⇒ Boolean): List[A] =
    flatMap(as)((a: A) ⇒ if (p(a)) List(a) else Nil)
  assert(filterFl(l)(_ % 2 == 0) == List(2, 4, 6))

  // Exercise 3.23
  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) ⇒ A): List[A] = (xs, ys) match {
    case (Cons(a, as), Cons(b, bs)) ⇒ Cons(f(a, b), zipWith(as, bs)(f))
    case _                          ⇒ Nil
  }
  assert(zipWith(l, l2)(_ + _) == List(8, 10, 12))

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil)                   ⇒ true
    case (Nil, _)                   ⇒ false
    case (Cons(x, xs), Cons(y, ys)) ⇒ (x == y && hasSubsequence(xs, ys)) || hasSubsequence(xs, sub)
  }

  assert(hasSubsequence(l, Nil))
  assert(hasSubsequence(Nil, Nil))
  assert(hasSubsequence(l, List(3, 4)))
  assert(!hasSubsequence(Nil, List(3, 4)))
  assert(!hasSubsequence(l, List(4, 3)))

  //
  // Trees
  //
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val leaf1 = Leaf(42)
  val leaf2 = Leaf(123)
  val branch1 = Branch(leaf1, leaf2)
  val branch2 = Branch(Leaf(101), Leaf(102))
  val branch3 = Branch(branch1, branch2)
  val t2 = Branch

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)             ⇒ 1
    case Branch(left, right) ⇒ 1 + size(left) + size(right)
  }

  assert(size(leaf1) == 1)
  assert(size(branch1) == 3)
  assert(size(branch3) == 7)

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value)         ⇒ value
    case Branch(left, right) ⇒ maximum(left) max maximum(right)
  }
  assert(maximum(leaf1) == 42)
  assert(maximum(branch3) == 123)

  // Exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)             ⇒ 1
    case Branch(left, right) ⇒ 1 + (depth(left) max depth(right))
  }

  assert(depth(leaf1) == 1)
  assert(depth(branch1) == 2)
  assert(depth(branch3) == 3)

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(v)             ⇒ Leaf(f(v))
    case Branch(left, right) ⇒ Branch(map(left)(f), map(right)(f))
  }

  assert(map(leaf1)(_.toString) == Leaf("42"))
  assert(map(branch3)(_.toString) == Branch(Branch(Leaf("42"), Leaf("123")), Branch(Leaf("101"), Leaf("102"))))

  // Exercise 3.29
  //  def fold[A, B](t: Tree[A], z: B)(f: (B, A) ⇒ B, comb: (B, B) ⇒ B): B = t match {
  //    case Leaf(v)             ⇒ f(z, v)
  //    case Branch(left, right) ⇒ comb(fold(left, z)(f, comb), fold(right, z)(f, comb)) // fold(left, fold(right, z)(f))(f)
  //  }

  def fold[A, B](t: Tree[A], z: B)(f: (B, A) ⇒ B, g: (B, B) ⇒ B): B = t match {
    case Leaf(v)             ⇒ g(z, f(z, v))
    case Branch(left, right) ⇒ ??? //g(fold(left, f(z + 1)(f, g), fold(right, z + 1)(f, g))
  }

  def depthF[A](t: Tree[A]): Int = fold(t, 0)((acc: Int, _: A) ⇒ acc + 1, (x: Int, y: Int) ⇒ x max y)
  assert(depthF(leaf1) == 1)
  println(s">>>> ${depthF(branch1)}")
  assert(depthF(branch1) == 2)
  println(s">>>> ${depthF(branch3)}")
  assert(depthF(branch3) == 3)

  def sizeF[A](t: Tree[A]): Int = fold(t, 0)((acc: Int, _: A) => acc + 1, (x: Int, y: Int) => x + y)

  def maximumF(t: Tree[Int]): Int = fold(t, Int.MinValue)((acc: Int, a: Int) => acc max a, (x: Int, y: Int) => x + y)
  
  // Could perhaps try to do the version of fold that works for size/maximum/depth?
  // And then see how I could adapt it to the map case too?
  //def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a: A => Leaf(f(a)), (x: B, y: B) => Branch(x, y)
    
}
