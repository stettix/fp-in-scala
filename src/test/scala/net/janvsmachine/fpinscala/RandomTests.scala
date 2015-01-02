package net.janvsmachine.fpinscala

import org.scalacheck.Gen
import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.PropSpec

import RNG._

class RandomTests extends PropSpec with Checkers {

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
