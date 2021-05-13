package net.janvsmachine.fpinscala.parsing

import fpinscala.testing.Prop.forAll
import fpinscala.testing.{Gen, Prop, SGen}

import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parsers[Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def errorMessage(pe: ParseError): String

  // Fundamental combinators

  implicit def string(s: String): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def regex(r: Regex): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]


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
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, as) => a :: as)

  def optional[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)).or(succeed(None))

  def optionalOr[A](p: Parser[A], default: => A): Parser[A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    map2(p, listOfN(n - 1, p))(_ :: _)

  def first[A, B](p: Parser[(A, B)]): Parser[A] =
    p.map { case (a, _) => a }

  def last[A, B](p: Parser[(A, B)]): Parser[B] =
    p.map { case (_,b) => b }

  def takeMiddle[A, B, C](p: Parser[((A, B), C)]): Parser[B] =
    p.map { case ((_, m), _) => m }

  def dropMiddle[A, B, C](p: Parser[((A, B), C)]): Parser[(A, C)] =
    p.map { case ((a, _), c) => (a, c) }

  implicit def operators[A](p: Parser[A]): ParserOps[A]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p1, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def optional: Parser[Option[A]] = self.optional(p)
    def optionalOr(default: => A): Parser[A] = self.optionalOr(p, default)
    def slice: Parser[String] = self.slice(p)
  }

  case class ParseError(stack: List[(Location, String)])

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
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

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _ => true
        }
      }

  }

}
