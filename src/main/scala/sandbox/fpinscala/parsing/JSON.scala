package sandbox.fpinscala.parsing

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    val spaces: Parser[String] =
      char(' ').many.slice

    val jNullParser: Parser[JSON] =
      string("null").map(_ => JNull)

    val jBoolParser: Parser[JSON] =
      (string("true") | string("false")).map(s => JBool(s.toBoolean))

    val jNumberParser: Parser[JSON] =
      "([-+]?\\d+)|([-+]?\\d+\\.\\d+)".r.map(s => JNumber(s.toDouble))

    val stringLiteral: Parser[String] = "[^\"]+".r

    val jStringParser: Parser[JSON] =
      (char('"') *> stringLiteral <* char('"')).map(JString)

    val commaSpaceSep: Parser[Char] = spaces *> char(',') <* spaces

    val jArrayParser: Parser[JSON] =
      (char('[') *> spaces *>
        jsonParser(P).sepBy(commaSpaceSep) <*
        spaces <* char(']')).map(l => JArray(l.toIndexedSeq))

    val pairs: Parser[(String, JSON)] =
      (stringLiteral <* spaces <* char(':')) ** (spaces *> jsonParser(P))

    val jObjectParser: Parser[JSON] =
      (char('{') *> spaces *>
        pairs.sepBy(commaSpaceSep) <*
        spaces <* char('}')).map(l => JObject(l.toMap))

    jNullParser | jBoolParser | jNumberParser | jStringParser | jArrayParser | jObjectParser
  }
}
