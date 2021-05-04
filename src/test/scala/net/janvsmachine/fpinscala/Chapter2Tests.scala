package net.janvsmachine.fpinscala

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.PropSpec
import Chapter2._
import org.scalatestplus.scalacheck

class Chapter2Tests extends PropSpec with org.scalatestplus.scalacheck.Checkers {

  val lt = (x: Int, y: Int) => x > y

  property("isSorted returns true for all sorted arrays") {
    check {
      forAll { (a: Array[Int]) =>
        isSorted(a.sortWith(lt), lt)
      }
    }
  }

  property("isSorted returns false for all reverse ranges") {
    forAll { (i: Int) =>
      i > 0 && i < 1000 ==> !isSorted((i to 0).toArray, lt)
    }
  }

}
