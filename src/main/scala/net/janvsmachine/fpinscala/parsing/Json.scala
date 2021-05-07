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

    // TODO: Whitespace!
    val spaces: Parser[String] = char(' ').many.slice

    val name: Parser[String] = regex("\\w".r)

    val quote: Parser[Char] = char('"')

    val identifier: Parser[String] =
      takeMiddle(quote ** name ** quote)

    val numberLiteral: Parser[JNumber] =
      (regex("\\d+".r) ** regex("\\.\\d+".r).optionalOr(""))
        .map { case (first, last) => JNumber(s"$first.$last".toDouble) }

    val stringLiteral: Parser[JString] =
      takeMiddle(quote ** regex("^[\"]".r) ** quote)
        .map(JString)

    val booleanLiteral: Parser[JBool] =
      (string("true") | string("True") | string("false") | string("False"))
        .map(b => JBool(b.toBoolean))

    lazy val jsonValue: Parser[Json] = numberLiteral | stringLiteral | booleanLiteral | array | jsonValue

    lazy val literalAndSeparator: Parser[Json] =
      first(jsonValue ** char(','))

    lazy val arrayMembers: Parser[List[Json]] =
      (many(literalAndSeparator) ** jsonValue)
        .map { case (ls, l) => ls ++ List(l) }
        .or(succeed(List.empty))

    lazy val array: Parser[JArray] =
      takeMiddle(char('[') ** (arrayMembers | succeed(List.empty)) ** char(']'))
        .map(ls => JArray(ls.toVector))

    lazy val objectKeyValue: Parser[(String, Json)] =
      dropMiddle(identifier ** char(':') ** jsonValue)

    lazy val objectMembers: Parser[List[(String, Json)]] =
      many(first(objectKeyValue ** char(','))) or objectKeyValue.map(List(_)) or succeed(List.empty)

    lazy val jsonObject: Parser[JObject] =
      takeMiddle(char('{') ** objectMembers.map(m => JObject(m.toMap)) ** char('}'))

    lazy val jsonTopLevel: Parser[Json] =
      array | jsonObject

    ???
  }
}
