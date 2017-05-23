package json

import fastparse.all._

object JsonParser {

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T) = f(t)
    override def toString() = name
  }

  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space         = P(CharsWhileIn(" \r\n").?)
  val digits        = P(CharsWhileIn("0123456789"))
  val exponent      = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
  val fractional    = P("." ~ digits)
  val integral      = P("0" | CharIn('1' to '9') ~ digits.?)

  val number = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(
    n => JsNumber(BigDecimal(n))
  )

  val `null`        = P("null" ).map(_ => JsNull)
  val `false`       = P("false").map(_ => JsBool(false))
  val `true`        = P("true" ).map(_ => JsBool(true))

  val hexDigit      = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val unicodeEscape = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  val escape        = P("\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape))

  val strChars = P(CharsWhile(StringChars))
  val string =
    P(space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(JsString)

  val array =
    P("[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(a => JsArray(a.toList))

  val pair = P(string.map(_.value) ~/ ":" ~/ jsonExpr)

  val obj =
    P("{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(a => JsObject(a.toMap))

  val jsonExpr: P[JsValue] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )

  def parse(input: String): Either[String, JsValue] =
    jsonExpr.parse(input) match {
      case Parsed.Success(value, _)  => Right(value)
      case f: Parsed.Failure => Left(s"Could not parse json: ${f.msg}")
    }
}
