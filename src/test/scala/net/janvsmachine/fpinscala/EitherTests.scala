package net.janvsmachine.fpinscala

import org.scalatest.FlatSpec

class EitherTests extends FlatSpec {

  import Either._

  val ex = new Exception("Test exception")
  val otherEx = new Exception("Other test exception")

  "An error value" should "comply with spec" in {
    val l: Either[Exception, String] = Left(ex)

    assert(l.map(_.length) == l)

    assert(l.flatMap(v ⇒ Right(v.length)) == l)
    assert(l.flatMap(v ⇒ Left(otherEx)) == l)

    assert(l.map2(Right("other"))((a, b) ⇒ "other") == l)
    assert(l.map2(Left(otherEx))((a, b) ⇒ "other") == l)

    assert(l.orElse { Right(42) } == Right(42))
    assert(l.orElse { Left(otherEx) } == Left(otherEx))
  }

  "A successful value" should "comply with spec" in {
    val r: Either[Exception, String] = Right("value")

    assert(r.map(_.length) == Right(5))

    assert(r.flatMap(v ⇒ Right(v.length)) == Right(5))
    assert(r.flatMap(v ⇒ Left(ex)) == Left(ex))

    assert(r.map2(Right("other"))((a, b) ⇒ a + b) == Right("valueother"))
    assert(r.map2(Left(ex))((a, b) ⇒ a + b) == Left(ex))

    assert(r.orElse { Right(42) } == r)
    assert(r.orElse { Left(otherEx) } == r)
  }

}
