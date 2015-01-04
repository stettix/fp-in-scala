package net.janvsmachine.fpinscala

import Math._
import List._

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object RNG {

  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng ⇒ (a, rng)

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    rng ⇒ {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng ⇒
      foldLeft(fs, (List[A](), rng))((acc: (List[A], RNG), r: Rand[A]) ⇒ {
        val (nextVal, nextRng) = r(rng)
        (Cons(nextVal, acc._1), nextRng)
      })

  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    val randB = g(a)
    randB(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i ⇒
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def ints2(count: Int): Rand[List[Int]] = {
    val randInts = List(scala.collection.immutable.List.fill(count)(int): _*)
    sequence(randInts)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val nextVal = if (n == Integer.MIN_VALUE) Integer.MAX_VALUE else abs(n)
    (nextVal, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    val nextVal = n.toDouble / Integer.MAX_VALUE
    (nextVal, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val ns = Stream.from(0).take(count).toList
    foldLeft(ns, (List[Int](), rng))((acc: (List[Int], RNG), _) ⇒ {
      val (nextVal, nextRng) = acc._2.nextInt
      (Cons(nextVal, acc._1), nextRng)
    })
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i ⇒ i - i % 2)

  def doublev2: Rand[Double] = map(nonNegativeInt)(_.toDouble / Integer.MAX_VALUE)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)
  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

}
