package net.janvsmachine.fpinscala

import java.util.concurrent.Executors
import org.scalatest.FlatSpec

import Par._

class ParTest extends FlatSpec {

  val es = Executors.newSingleThreadExecutor()

  "A parallel computation" should "be have a unit constructor" in {
    assert(eval(unit(42)) == 42)
  }

  it should "also have a forking constructor" in {
    assert(eval(fork(unit(42))) == 42)
  }

  it should "allow combingin with another parallel computation" in {
    val a = unit(42)
    val b = unit("foo")
    val res = map2(a, b)((_, _))
    assert(eval(res) == (42, "foo"))
  }

  it should "allow mapping over the result" in {
    val p = map(unit(42))(_.toString)
    assert(eval(p) == "42")
  }

  it should "allow sequencing of results" in {
    val p = sequence(List(unit(1), unit(2), unit(3)))
    assert(eval(p) == List(1, 2, 3))
  }

  private def eval[A](p: Par[A]): A = Par.run(es)(p).get

}
