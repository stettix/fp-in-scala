package net.janvsmachine.fpinscala

import org.scalacheck.Properties
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.PropSpec
import org.scalatest.FlatSpec

class Chapter4Tests extends FlatSpec {

  import Option._

  val none: Option[String] = None
  val value: Option[String] = Some("42")

  "A None value" should "comply with contract" in {
    assert(none.map(_.length) == None)
    assert(none.flatMap(v ⇒ None) == None)
    assert(none.flatMap(v ⇒ Some(v.length)) == None)
    assert(none.getOrElse("foo") == "foo")
    assert(none.orElse("foo") == Some("foo"))
    assert(none.filter(v ⇒ true) == None)
    assert(none.filter(v ⇒ false) == None)
  }

  "A Some() value" should "comply with contract" in {
    assert(value.map(_.length) == Some(2))
    assert(value.flatMap(v ⇒ None) == None)
    assert(value.flatMap(v ⇒ Some(v.length)) == Some(2))
    assert(value.getOrElse("foo") == "42")
    assert(value.orElse("foo") == value)
    assert(value.filter(v ⇒ true) == value)
    assert(value.filter(v ⇒ false) == None)
  }

  "The variance of a sequence" should "be None for an empty sequence" in {
    assert(variance(Seq()) == None)
  }

  it should "be defined for a non-empty sequence" in {
    assert(variance(Seq(1.0, 2.0, 3.0)) == Some((1.0 + 0.0 + 1.0) / 3))
  }

  "Sequence function" should "combine values when list values are all defined" in {
    assert(sequence(List()) == Some(List()))
    assert(sequence(List(Some(1))) == Some(List(1)))
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

  it should "return None if any value in the list is None" in {
    assert(sequence(List(None)) == None)
    assert(sequence(List(None, Some(2), Some(3))) == None)
    assert(sequence(List(Some(1), None, Some(3))) == None)
    assert(sequence(List(Some(1), Some(2), None)) == None)
  }

  "Traverse function" should "combine and map values when all list values are defined" in {
    assert(traverse(List())(_.toString) == Some(List()))
    assert(traverse(List(Some(1)))(_.toString) == Some(List("1")))
    assert(traverse(List(Some("1"), Some("2"), Some("3")))(_.toString) == Some(List("1", "2", "3")))
  }

  it should "return None if any value in the list is None" in {
    assert(traverse(List(None))(_.toString) == None)
    assert(traverse(List(None, Some(2), Some(3)))(_.toString) == None)
    assert(traverse(List(Some(1), None, Some(3)))(_.toString) == None)
    assert(traverse(List(Some(1), Some(2), None))(_.toString) == None)
  }

}

class MapTests extends PropSpec with Checkers {

  import Option._

  val f = (x: Int, y: Int) ⇒ x + y

  property("Alternative implementations map should always give some result") {
    check {
      forAll { (a: Int, b: Int) ⇒
        // Not very clever - would be nice if PropSpec could generate optional values - but seems it can't?
        map2(Some(a), Some(b))(f) == map2a(Some(a), Some(b))(f)
        map2(None, Some(b))(f) == map2a(None, Some(b))(f)
        map2(Some(a), None)(f) == map2a(Some(a), None)(f)
      }
    }
  }

}
