package json

import org.scalatest.{FunSuite, Matchers}

import cats.implicits._

import Json._

class JsonExample extends FunSuite with Matchers {

  case class Device(id: Int, name: String, values: List[String], valid: Boolean)

  val fromJsonForTuple1 =
    new FromJson[(Int, String, List[String], Boolean)] {
      override def fromJson1 =
        withObject("Tuple of 4") { o =>
          for {
            id     <- o \[Int]          "id"
            name   <- o \[String]       "name"
            values <- o \[List[String]] "values"
            valid  <- o \[Boolean]      "valid"
          } yield (id, name, values, valid)
        }
    }

  val fromJsonForTuple2 =
    new FromJson[(Int, String, List[String], Boolean)] {
      override def fromJson1 =
        withObject("Tuple of 4") { o =>
          (   o \[Int]          "id"
          |@| o \[String]       "name"
          |@| o \[List[String]] "values"
          |@| o \[Boolean]      "valid"
          ).map((a, b, c, d) => (a, b, c, d))
        }
    }

  val fromJsonForDevice =
    new FromJson[Device] {
      override def fromJson1 =
        withObject("Device object") { o =>
          (   o \[Int]          "id"
          |@| o \[String]       "name"
          |@| o \[List[String]] "values"
          |@| o \[Boolean]      "valid"
          ).map(Device.apply)
        }
    }

  val toJsonForDevice =
    new ToJson[Device] {
      override def toJson1(d: Device) =
        JsObject(Map( "id"     -> JsNumber(d.id)
                    , "name"   -> JsString(d.name)
                    , "values" -> JsArray(d.values.map(JsString(_)))
                    , "valid"  -> JsBool(d.valid)
                    ))
    }

  val toJsonForDevice2 =
    new ToJson[Device] {
      override def toJson1(d: Device) =
        JsObject(Map( "id"     -> toJson(d.id)
                    , "name"   -> toJson(d.name)
                    , "values" -> toJson(d.values)
                    , "valid"  -> toJson(d.valid)
                    ))
    }

  val toJsonForDevice3 =
    new ToJson[Device] {
      override def toJson1(d: Device) =
        obj( "id"     -> toJson(d.id)
           , "name"   -> toJson(d.name)
           , "values" -> toJson(d.values)
           , "valid"  -> toJson(d.valid)
           )
    }

  test("Json should parse tuple 1") {
    implicit val a = fromJsonForTuple1
    decodeEither[(Int, String, List[String], Boolean)]("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""") shouldEqual
      Right((1, "nameVal", List("one", "two"), true))
  }

  test("Json should parse tuple 2") {
    implicit val a = fromJsonForTuple2
    decodeEither[(Int, String, List[String], Boolean)]("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""") shouldEqual
      Right((1, "nameVal", List("one", "two"), true))
  }

  test("Json should parse Device") {
    implicit val a = fromJsonForDevice
    decodeEither[Device]("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""") shouldEqual
      Right(Device(1, "nameVal", List("one", "two"), true))
  }

  test("Json should fail to parse Device with bad json") {
    implicit val a = fromJsonForDevice
    decodeEither[Device]("""{""") shouldEqual
      Left("Could not parse json: \"}\":1:2 ...\"\"")
  }

  test("Json should fail to parse Device with wrong type") {
    implicit val a = fromJsonForDevice
    decodeEither[Device]("""[]""") shouldEqual
      Left("expected Device object, encountered Array")
  }

  test("Json should fail to parse tuple with missing key") {
    implicit val a = fromJsonForTuple1
    decodeEither[(Int, String, List[String], Boolean)]("""{"id": 1, "name1": "nameVal", "values": ["one", "two"], "valid": true}""") shouldEqual
      Left("key 'name' not present")
  }

  test("Json should fail to parse tuple with wrong type") {
    implicit val a = fromJsonForTuple1
    decodeEither[(Int, String, List[String], Boolean)]("""{"id": "1", "name": "nameVal", "values": ["one", "two"], "valid": true}""") shouldEqual
      Left("expected Number, encountered String")
  }

  test("Json should encode json for manual toJsonForDevice") {
    implicit val a = toJsonForDevice
    encode(Device(1, "nameVal", List("one", "two"), true)) shouldEqual
      """{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}"""
  }

  test("Json should encode json for toJsonForDevice depending on implicit toJsons") {
    implicit val a = toJsonForDevice2
    encode(Device(1, "nameVal", List("one", "two"), true)) shouldEqual
      """{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}"""
  }

  test("Json should encode json for toJsonForDevice with helper syntax") {
    implicit val a = toJsonForDevice3
    encode(Device(1, "nameVal", List("one", "two"), true)) shouldEqual
      """{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}"""
  }

  test("Json should enable parsing string to jsValue, and jsValue to model") {
    implicit val a = fromJsonForDevice
    val eitherJsValue = JsonParser.parse("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""")
    eitherJsValue shouldEqual
      Right(JsObject(Map("id" -> JsNumber(1),
                         "name" -> JsString("nameVal"),
                         "values" -> JsArray(List(JsString("one"), JsString("two"))),
                         "valid" -> JsBool(true))))
    val Right(jsValue) = eitherJsValue
    parseJson[Device](jsValue) shouldEqual
      Success(Device(1, "nameVal", List("one", "two"), true))
  }
}
