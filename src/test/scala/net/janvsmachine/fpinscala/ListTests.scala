package net.janvsmachine.fpinscala

import org.scalatest.FlatSpec

import List._

class ListTests extends FlatSpec {

  val l = List(1, 2, 3, 4, 5, 6)
  val l2 = List(7, 8, 9)

  it should "implement sum" in {
    assert(sum(l) == 21)
  }

  it should "implement product" in {
    assert(product(l) == 2 * 3 * 4 * 5 * 6)
  }

  it should "implement reverse using fold" in {
    assert(reverse(l) == List(6, 5, 4, 3, 2, 1))
  }

  it should "implement foldLeft" in {
    assert(foldLeft(l, 0)(_ + _) == foldLeftR(l, 0)(_ + _))
  }

  it should "implement foldRight1 and foldRight (the latter using foldLeft)" in {
    assert(foldRight(l, 0)(_ + _) == sum(l))
    assert(foldRight1(l, 0)(_ + _) == foldRight(l, 0)(_ + _))
  }

  // Exercise 3.8.
  it should "be able to copy list using foldRight and Nil + Cons" in {
    assert(foldRight(l, Nil.asInstanceOf[List[Int]])(Cons(_, _)) == l,
      "Using Nil and Cons as args to foldLeft should make copy of list")
  }

  it should "implement flatMap" in {
    assert(flatMap(l)(x => List(x, x)) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
  }

  it should "implement append" in {
    assert(append(l, l2) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  it should "implement concat" in {
    assert(concat(Nil) == Nil)
    assert(concat(List(Nil, l2)) == l2)
    assert(concat(List(l, l2)) == append(l, l2))
  }

  it should "append add1" in {
    assert(add1(l) == List(2, 3, 4, 5, 6, 7))
  }

  it should "implement map" in {
    assert(map(l)(_ + 1) == List(2, 3, 4, 5, 6, 7))
  }

  it should "implement filter" in {
    assert(filter(l)(_ % 2 == 0) == List(2, 4, 6))
  }

  it should "implement filter using flatMap" in {
    assert(filterFl(l)(_ % 2 == 0) == List(2, 4, 6))
  }

  it should "implement zipWith" in {
    assert(zipWith(l, l2)(_ + _) == List(8, 10, 12))
  }

  it should "implement hasSubsequence" in {
    assert(hasSubsequence(l, Nil))
    assert(hasSubsequence(Nil, Nil))
    assert(hasSubsequence(l, List(3, 4)))
    assert(!hasSubsequence(Nil, List(3, 4)))
    assert(!hasSubsequence(l, List(4, 3)))
  }

  it should "implement length" in {
    assert(length(Nil) == 0)
    assert(length(l) == 6)
    assert(length(l2) == 3)
  }

}
