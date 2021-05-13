package net.janvsmachine.fpinscala.parsing

sealed trait Json

object Json {
  case class JNull() extends Json // Had to make this a class instead of object to make Parser[JNull] work
  case class JNumber(get: Double) extends Json
  case class JString(get: String) extends Json
  case class JBool(get: Boolean) extends Json
  case class JArray(get: IndexedSeq[Json]) extends Json
  case class JObject(get: Map[String, Json]) extends Json

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    // TODO: Whitespace!
    val ws: Parser[String] = regex("\\s".r)

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
      (string("true") | string("false"))
        .map(b => JBool(b.toBoolean))

    val nullLiteral: Parser[JNull] =
      string("null").map(_ => JNull())

    lazy val array: Parser[JArray] =
      ((char('[') ** ws ** char(']')).map(_ => List.empty) or
        takeMiddle(char('[') ** elements ** char(']')))
        .map(es => JArray(es.toVector))

    lazy val element: Parser[Json] = takeMiddle(ws ** value ** ws)

    lazy val elements: Parser[List[Json]] =
      element.map(List(_)) or
        (first(element ** char(',')).map(List(_)) ** elements).map { case (a, b) => a ++ b }

    lazy val member: Parser[(String, Json)] =
      dropMiddle(takeMiddle(ws ** identifier ** ws) ** char(':') ** element)

    lazy val members: Parser[List[(String, Json)]] =
      member.map(List(_)) or
        (first(member ** char(',')).map(List(_)) ** members).map { case (a, b) => a ++ b }

    lazy val jsonObject: Parser[JObject] =
      ((char('{') ** ws ** char('{')).map(_ => List.empty) or
        takeMiddle(char('{') ** members ** char('}')))
        .map(m => JObject(m.toMap))

    lazy val value: Parser[Json] = jsonObject | array | stringLiteral | numberLiteral | booleanLiteral | nullLiteral

    val json = element

    json
  }
}
