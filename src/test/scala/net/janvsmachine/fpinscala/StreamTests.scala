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

  "exists" should "do the right thing yea" in {
    assert(Stream(1, 2, 3).exists(_ > 2))
    assert(!empty[Int].exists(_ > 0))
  }

  "forAll" should "you know" in {
    assert(empty[Int].forAll(_ ⇒ false))
    assert(Stream(1, 2, 3).forAll(_ > 0))
    assert(!Stream(1, 2, 3).forAll(_ < 3))
  }

  "takeWhile" should "behave itself too" in {
    assert(empty[Int].takeWhile(_ < 3) == empty)
    assert(Stream(1, 2, 3).takeWhile(_ < 3).toList == List(1, 2))
  }

}
