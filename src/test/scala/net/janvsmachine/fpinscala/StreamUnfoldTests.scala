package net.janvsmachine.fpinscala

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.PropSpec
import Stream._

/**
 * Tests for the Stream methods re-implemented using unfold.
 * These should all behave as their explicitly implemented counterparts.
 */
class StreamUnfoldTests extends PropSpec with Checkers {

  def sizes = Gen.choose(0, 100)

  property("constant2 is equivalent to constant") {
    check {
      forAll(sizes) { (n: Int) ⇒
        constant("foo").take(n).toList == constant2("foo").take(n).toList
      }
    }
  }

  property("fibs2 is equivalent to fibs") {
    check {
      forAll(sizes) { (n: Int) ⇒
        fibs.take(n).toList == fibs2.take(n).toList
      }
    }
  }

  property("from2 is equivalent to from") {
    check {
      forAll { (start: Int) ⇒
        forAll(sizes) { (n: Int) ⇒
          from(start).take(n).toList == from2(start).take(n).toList
        }
      }
    }
  }

  property("map2 is equivalent to map") {
    check {
      forAll(sizes) { (n: Int) ⇒
        fibs.take(n).map(_.toString).toList == fibs.take(n).map2(_.toString).toList
      }
    }
  }

}
