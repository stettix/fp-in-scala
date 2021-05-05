package net.janvsmachine.fpinscala

import fpinscala.testing.Prop._
import fpinscala.testing._

import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+ _]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // Fundamental combinators

  implicit def string(s: String): Parser[String]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  // Derived combinators

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(f andThen succeed)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p1.flatMap(a => p2.map(a -> _))

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def map2[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    for { a <- p1; b <- p2 } yield f(a, b)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(Cons(_, _)) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, as) => Cons(a, as))

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, listOfN(n - 1, p))(Cons(_, _))

  implicit def operators[A](p: Parser[A]): ParserOps[A]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p1, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many(p: Parser[A]): Parser[List[A]] = self.many(p)
    def many1(p: Parser[A]): Parser[List[A]] = self.many1(p)
    def slice(p: Parser[A]): Parser[String] = self.slice(p)
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
