package net.janvsmachine.fpinscala

import org.scalatest.FlatSpec

class Chapter4Tests extends FlatSpec {

  import Chapter4._

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

}
