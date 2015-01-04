package net.janvsmachine.fpinscala

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalatest.PropSpec
import List._
import RNG._

class RandomPropSpecs extends PropSpec with Checkers {

  property("nonNegativeInt returns values in the correct range") {
    check {
      forAll { (n: Int) ⇒
        nonNegativeInt(SimpleRNG(n))._1 >= 0
      }
    }
  }

  property("double returns values in the correct range") {
    check {
      forAll { (n: Int) ⇒
        val d = double(SimpleRNG(n))._1
        d >= 0 && d < 1.0
      }
    }
  }

}

class RandomTests extends FlatSpec {

  "ints" should "generate the right number of values" in {
    val rng = SimpleRNG(42)
    assert(ints(0)(rng)._1 == Nil)
    assert(length(ints(1)(rng)._1) == 1)
    assert(length(ints(10)(rng)._1) == 10)
  }

}
