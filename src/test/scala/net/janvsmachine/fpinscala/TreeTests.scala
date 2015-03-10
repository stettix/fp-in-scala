package net.janvsmachine.fpinscala

import org.scalatest.FlatSpec

import Tree._

class TreeTests extends FlatSpec {

  val leaf1 = Leaf(42)
  val leaf2 = Leaf(123)
  val branch1 = Branch(leaf1, leaf2)
  val branch2 = Branch(Leaf(101), Leaf(102))
  val branch3 = Branch(branch1, branch2)

  "A Tree" should "implement size" in {
    assert(size(leaf1) == 1)
    assert(size(branch1) == 3)
    assert(size(branch3) == 7)
  }

  it should "implement maximum" in {
    assert(maximum1(leaf1) == 42)
    assert(maximum1(branch3) == 123)
  }

  it should "implement maximum via fold" in {
    assert(maximum(leaf1) == 42)
    assert(maximum(branch3) == 123)
  }

  it should "implement depth" in {
    assert(depth1(leaf1) == 0)
    assert(depth1(branch1) == 1)
    assert(depth1(branch3) == 2)
  }

  it should "implement depth via fold" in {
    assert(depth(leaf1) == 0)
    assert(depth(branch1) == 1)
    assert(depth(branch3) == 2)
  }

  it should "implement map" in {
    assert(map(leaf1)(_.toString) == Leaf("42"))
    assert(map(branch3)(_.toString) == Branch(Branch(Leaf("42"), Leaf("123")), Branch(Leaf("101"), Leaf("102"))))
  }

}
