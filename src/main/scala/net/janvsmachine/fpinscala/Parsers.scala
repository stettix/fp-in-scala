package net.janvsmachine.fpinscala

import fpinscala.testing.Prop._
import fpinscala.testing._

import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+ _]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def string(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  // Returns the number of repeated values of the given parser seen
  def zeroOrMore[A](p: Parser[A]): Parser[Int]

  // Returns the number of repeated values of the given parser seen
  def oneOrMore[A](p: Parser[A]): Parser[Int]

  def zeroOrMoreThenOneOrMore[A](p1: Parser[A], p2: Parser[A]): (Int, Int)

  // Fundamental combinators

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, listOfN(n - 1, p))(Cons(_, _))

  // Return the part of a string that the parser matches
  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  // Derived combinators

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    product(p1, p2).map(f.tupled)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(Cons(_, _)) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, as) => Cons(a, as))

  // Law: for any char 'c', run(char(c))(c.toString) should return Right(c)
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  implicit def operators[A](p: Parser[A]): ParserOps[A]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def many(p: Parser[A]): Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(42))(s) == Right(42))

    def stringLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s) == Right(s))

  }

}
