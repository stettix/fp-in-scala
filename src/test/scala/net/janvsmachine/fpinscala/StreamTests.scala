package net.janvsmachine.fpinscala

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import Stream._

@RunWith(classOf[JUnitRunner])
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

  it should "implement forAll" in {
    assert(empty[Int].forAll(_ ⇒ false))
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
    assert(empty().filter(_ ⇒ false) == empty)
    assert(empty().filter(_ ⇒ true) == empty)
    assert(Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList == List(2, 4))
  }

  it should "implement append" in {
    assert(empty().append(empty()) == empty)
    assert(Stream(1).append(empty()).toList == List(1))
    assert(empty[Int]().append(Stream(1)).toList == List(1))
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
  }

}
