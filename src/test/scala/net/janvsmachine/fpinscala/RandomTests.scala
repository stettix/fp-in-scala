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
        //SimpleRNG(n).nextInt._1 <= 99999999
      }
    }
  }

}
