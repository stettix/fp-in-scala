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
    assert(maximum(leaf1) == 42)
    assert(maximum(branch3) == 123)
  }

  it should "implement depth" in {
    assert(depth(leaf1) == 1)
    assert(depth(branch1) == 2)
    assert(depth(branch3) == 3)
  }

  it should "implement map" in {
    assert(map(leaf1)(_.toString) == Leaf("42"))
    assert(map(branch3)(_.toString) == Branch(Branch(Leaf("42"), Leaf("123")), Branch(Leaf("101"), Leaf("102"))))
  }

  // TODO!!!
  ignore should "implement depth using fold" in {
    assert(depthF(leaf1) == 1)
    assert(depthF(branch1) == 2)
    assert(depthF(branch3) == 3)
  }

}
