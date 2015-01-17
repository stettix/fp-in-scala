package net.janvsmachine.fpinscala

import java.util.concurrent.Executors
import org.scalatest.FlatSpec

import Par._

class ParTest extends FlatSpec {

  val es = Executors.newCachedThreadPool()

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

  "parFilter" should "work as filter for any list" in {
    def isEven(n: Int) = n % 2 == 0

    assert(eval(parFilter(Nil)(isEven)) == Nil)

    assert(eval(parFilter(List(2))(isEven)) == List(2))
    assert(eval(parFilter(List(1))(isEven)) == Nil)

    val l = List(1, 2, 3, 4, 5)
    val r = parFilter(l)(isEven)
    assert(eval(r) == List(2, 4))
  }

  val l = Vector(1, 2, 3, 8, 9, 10, 4, 5, 6, 7)

  "merge" should "merge values in parallel, using log(2) number of operations" in {
    val r = merge(l, 0)(_ + _)
    assert(eval(r) == l.foldLeft(0)(_ + _))
  }

  it should "find the max of a vector in parallel" in {
    val r = merge(l, Integer.MIN_VALUE)(_ max _)
    assert(eval(r) == 10)
  }

  it should "count the number of words in a sequence of paragraphs" in {
    val paragraphs = Vector("Lorem ipsum dolor sit amet",
      "consectetur adipiscing elit", "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
      "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
      "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
      "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum")

    def numWords(paragraph: String) = paragraph.split(" ").filter(_.length > 0).size

    //val wordCount = merge(paragraphs, 0)((w1, w2) ⇒ numWords(_) + numWords(_))
    // TODO!
  }

  it should "merge an empty sequence to the zero value" in {
    assert(eval(merge(Vector(), 0)(_ + _)) == 0)
  }

  "map3" should "map 3 parallel computations into 1 value" in {
    val res = map3(unit(1), unit(2), unit(3))((_, _, _))
    assert(eval(res) == (1, 2, 3))
  }

  "map4" should "map 4 parallel computations into 1 value" in {
    val res = map4(unit(1), unit(2), unit(3), unit(4))((_, _, _, _))
    assert(eval(res) == (1, 2, 3, 4))
  }

  private def eval[A](p: Par[A]): A = Par.run(es)(p).get

}
