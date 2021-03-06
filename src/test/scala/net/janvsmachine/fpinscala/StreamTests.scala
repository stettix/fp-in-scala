package net.janvsmachine.fpinscala

import org.scalatest.FlatSpec
import Stream._

class StreamTests extends FlatSpec {

  "An empty stream" should "comply with spec" in {
    assert(empty.toList == Nil)
    assert(empty.headOption == None)
  }

  "A stream with values" should "comply with spec" in {
    assert(Stream(1, 2, 3).toList == List(1, 2, 3))
    assert(Stream(1, 2, 3).headOption == Some(1))
  }

  "A stream" should "implement exists" in {
    assert(Stream(1, 2, 3).exists(_ > 2))
    assert(!empty[Int].exists(_ > 0))
  }

  it should "implement take" in {
    assert(empty.take(1) == empty)
    assert(Stream(1, 2, 3, 4).take(0) == empty)
    assert(Stream(1, 2, 3, 4).take(1).toList == List(1))
    assert(Stream(1, 2, 3, 4).take(2).toList == List(1, 2))
    assert(Stream(1, 2, 3, 4).take(10).toList == List(1, 2, 3, 4))
  }

  it should "implement drop" in {
    assert(empty.drop(1) == empty)
    assert(Stream(1, 2, 3, 4).drop(0).toList == List(1, 2, 3, 4))
    assert(Stream(1, 2, 3, 4).drop(1).toList == List(2, 3, 4))
    assert(Stream(1, 2, 3, 4).drop(2).toList == List(3, 4))
    assert(Stream(1, 2, 3, 4).drop(4) == empty)
    assert(Stream(1, 2, 3, 4).drop(10) == empty)
  }

  it should "implement forAll" in {
    assert(empty[Int].forAll(_ => false))
    assert(Stream(1, 2, 3).forAll(_ > 0))
    assert(!Stream(1, 2, 3).forAll(_ < 3))
  }

  it should "implement takeWhile" in {
    assert(empty[Int].takeWhile(_ < 3) == empty)
    assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))
  }

  it should "implement map" in {
    assert(empty[Int].map(_.toString) == empty)
    assert(Stream(1, 2, 3, 4).map(_.toString).toList == List("1", "2", "3", "4"))
  }

  it should "implement filter" in {
    assert(empty[String].filter(_ => false) == empty)
    assert(empty[String].filter(_ => true) == empty)
    assert(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList == List(2, 4))
  }

  it should "implement append" in {
    assert(empty.append(empty) == empty)
    assert(Stream(1).append(empty).toList == List(1))
    assert(empty.append(Stream(1)).toList == List(1))
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
  }

  it should "implement flatMap" in {
    def doubler[T] = (x: T) => Stream(x, x)
    assert(empty.flatMap(doubler) == empty)
    assert(Stream(1, 2, 3).flatMap(doubler).toList == List(1, 1, 2, 2, 3, 3))
  }

  it should "implement find" in {
    assert(empty[String].find(_ => false) == None)
    assert(empty[String].find(_ => true) == None)
    assert(Stream(1, 2, 3, 4).find(_ % 2 == 0) == Some(2))
  }

  it should "implement infinite stream functions" in {
    assert(constant(1).take(5).toList == List(1, 1, 1, 1, 1))
    assert(from(0).take(3).toList == List(0, 1, 2))
    assert(from(2).take(2).toList == List(2, 3))

    assert(fibs.take(1).toList == List(0))
    assert(fibs.take(2).toList == List(0, 1))
    assert(fibs.take(3).toList == List(0, 1, 1))
    assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  it should "implement unfold, and methods implemented in terms of unfold" in {
    assert(fibs.take(10).toList == fibs2.take(10).toList)
    assert(from(5).take(10).toList == from2(5).take(10).toList)

    assert(constant(5).take(10).toList == constant2(5).take(10).toList)
    assert(ones.take(5).toList == constant2(1).take(5).toList)
  }
}
