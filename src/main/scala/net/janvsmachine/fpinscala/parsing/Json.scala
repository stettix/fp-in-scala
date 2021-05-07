package net.janvsmachine.fpinscala.parsing

sealed trait Json

object Json {
  case object JNull extends Json
  case class JNumber(get: Double) extends Json
  case class JString(get: String) extends Json
  case class JBool(get: Boolean) extends Json
  case class JArray(get: IndexedSeq[Json]) extends Json
  case class JObject(get: Map[String, Json]) extends Json

  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[Json] = {
    import P._

    val spaces: Parser[String] = char(' ').many.slice
    val name: Parser[String] = regex("\\w".r)

    val quote: Parser[Char] = char('"')

    val identifier: Parser[String] =
      (quote ** name ** quote)
        .map { case ((_, id), _) => id }

    val numberLiteral: Parser[JNumber] =
      (regex("\\d+".r) ** regex("\\.\\d+".r).optionalOr(""))
        .map { case (first, last) => JNumber(s"$first.$last".toDouble) }

    val stringLiteral: Parser[JString] =
      (quote ** regex("^[\"]".r) ** quote)
        .map { case ((_, id), _) => JString(id) }

    val booleanLiteral: Parser[JBool] =
      (string("true") | string("True") | string("false") | string("False"))
        .map(b => JBool(b.toBoolean))

    lazy val literal: Parser[Json] = numberLiteral | stringLiteral | booleanLiteral | array | json

    lazy val literalAndSeparator: Parser[Json] = (literal ** char(','))
      .map { case (l, _) => l }

    lazy val arrayMembers: Parser[List[Json]] =
      (many(literalAndSeparator) ** literal)
        .map { case (ls, l) => ls ++ List(l) }

    lazy val array: Parser[JArray] =
      middle(char('[') ** (arrayMembers | succeed(List.empty)) ** char(']'))
        .map(ls => JArray(ls.toVector))

    lazy val objectKeyValue: Parser[(String, Json)] = ???

    lazy val objectKeyValueAndSeparator: Parser[(String, Json)] = ???

    lazy val objectMembers: Parser[List[(String, Json)]] = ???
      many(objectKeyValueAndSeparator) or optionalOr(objectKeyValue, List.empty) // <<--

    lazy val jsonObject: Parser[JObject] =
      middle(char('{') ** objectMembers.map(members => JObject(members.toMap)) ** char('}'))

    lazy val json: Parser[Json] =
      array | jsonObject

    ???
  }
}
