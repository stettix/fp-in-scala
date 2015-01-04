package net.janvsmachine.fpinscala

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalatest.PropSpec
import List._
import RNG._
import org.scalatest.prop.Whenever

class RandomPropSpecs extends PropSpec with Checkers with Whenever {

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

  property("nonNegativeLessThan generates values in the expected range") {
    check {
      forAll { (n: Int) ⇒
        n <= 0 || {
          val i = nonNegativeLessThan(n)(SimpleRNG(42))._1
          (i >= 0 && i < n)
        }
      }
    }
  }

}

class RandomTests extends FlatSpec {

  val rng = SimpleRNG(42)

  "ints" should "generate the right number of values" in {
    assert(ints(0)(rng)._1 == Nil)
    assert(length(ints(1)(rng)._1) == 1)
    assert(length(ints(10)(rng)._1) == 10)
  }

  "ints2" should "generate the right number of values" in {
    assert(ints2(0)(rng)._1 == Nil)
    assert(length(ints2(1)(rng)._1) == 1)
    assert(length(ints2(10)(rng)._1) == 10)
  }

}
