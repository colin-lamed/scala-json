package json.lens

import org.scalatest.{FunSuite, Matchers}
import json._
import json.Json._
import monocle.{Fold, Getter}
import cats.implicits._

import RLens._

class RLensExample extends FunSuite with Matchers {

  case class Device(id: Int, name: String, values: List[String], valid: Boolean)

  implicit val fromJsonForDevice =
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

  implicit val toJsonForDevice =
    new ToJson[Device] {
      override def toJson1(d: Device) =
        obj( "id"     -> toJson(d.id)
           , "name"   -> toJson(d.name)
           , "values" -> toJson(d.values)
           , "valid"  -> toJson(d.valid)
           )
    }

  def js(s: String): JsValue = JsonParser.parse(s).toOption.getOrElse(sys.error(s"Could not parse $s"))

  test("JsonPath work with as") {
    val f: Getter[JsValue, Result[Device]] = root.as[Device]

    f.get(js("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""")) shouldEqual
      Success(Device(1, "nameVal", List("one", "two"), true))

    f.get(js("{}")) shouldEqual Error(List(), "key 'id' not present")
  }

  test("JsonPath work with at") {
    val f: Getter[JsValue, Result[Device]] = root.at("key").as[Device]

    f.get(js("""{"key": {"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}}""")) shouldEqual
      Success(Device(1, "nameVal", List("one", "two"), true))

    f.get(js("""{"key2": {"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}}""")) shouldEqual
      Error(List(), "key 'key' not present")

    f.get(js("""{"key": []}""")) shouldEqual
      Error(List(), "expected Device object, encountered Array")

    f.get(js("""{"key": {"id": 1, "name": "nameVal", "values": "one", "valid": true}}""")) shouldEqual
      Error(List(Key("values")), "expected Array, encountered String")
  }

  test("JsonPath report path for nested model") {
    case class Model1(id: Int, name: String, values: List[String], valid: Boolean)
    case class Model2(values: List[Model1], value2: Model1)

    implicit val fromJsonForModel1 =
      new FromJson[Model1] {
        override def fromJson1 =
          withObject("Model1") { o =>
            (   o \[Int]          "id"
            |@| o \[String]       "name"
            |@| o \[List[String]] "values"
            |@| o \[Boolean]      "valid"
            ).map(Model1.apply)
          }
      }

    implicit val fromJsonForModel2 =
      new FromJson[Model2] {
        override def fromJson1 =
          withObject("Model2") { o =>
            (   o \[List[Model1]] "values"
            |@| o \[Model1]       "value2"
            ).map(Model2.apply)
          }
      }

    val f: Getter[JsValue, Result[Model2]] = root.as[Model2]

    f.get(js("""
      {
        "values":
          [ {"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
          , {"id": 2, "name": "nameVal2", "values": ["three", "four"], "valid": false}
          ],
        "value2":
          {"id": 3, "name": "nameVal3", "values": ["five", "six"], "valid": "true"}
      }""")) shouldEqual
      Error(List(Key("value2"), Key("valid")), "expected Boolean, encountered String")

    val f2: Getter[JsValue, Result[Model2]] = root.at("key").as[Model2]

    f2.get(js("""
      {"key":
        {
          "values":
            [ {"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
            , {"id": 2, "name": "nameVal2", "values": ["three", "four"], "valid": false}
            ],
          "value2":
            {"id": 3, "name": "nameVal3", "values": ["five", "six"], "valid": "true"}
        }
      }""")) shouldEqual
      Error(List(Key("value2"), Key("valid")), "expected Boolean, encountered String")
  }

  test("JsonPath work with index") {
    val f: Getter[JsValue, Result[Device]] = root.index(1).as[Device]

    f.get(js(
      """[{"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
         ,{"id": 2, "name": "nameVal2", "values": ["one", "two"], "valid": false}
         ,{"id": 3, "name": "nameVal3", "values": ["one", "two"], "valid": true}
         ]""")) shouldEqual
      Success(Device(2, "nameVal2", List("one", "two"), false))

    f.get(js("""[{"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}]""")) shouldEqual
      Error(List(), "index '1' not present")

    f.get(js(
      """[{"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
         ,{"id": 2, "name": "nameVal2", "values": ["one", "two"], "valid": "false"}
         ,{"id": 3, "name": "nameVal3", "values": ["one", "two"], "valid": true}
         ]""")) shouldEqual
      Error(List(Key("valid")), "expected Boolean, encountered String")
  }

  test("Lens should work with each") {
    val fold: Fold[JsValue, Result[JsValue]] = root.at("two").at("twob").each

    fold.length(js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")) shouldEqual 3
    fold.getAll(js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")) shouldEqual
      List(Success(JsString("b1")), Success(JsString("b2")), Success(JsString("b3")))
  }

  test("Lens should work with each and as") {
    case class MyObj(s: String)
    implicit val from =
      new FromJson[MyObj] {
        override def fromJson1 =
          withString("string") { s => success(MyObj(s)) }
      }

    val fold: Fold[JsValue, Result[MyObj]] = root.at("two").at("twob").each.as[MyObj]

    fold.getAll(js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")) shouldEqual
      List(Success(MyObj("b1")), Success(MyObj("b2")), Success(MyObj("b3")))
  }

  /*test("Lens should work with nested value - syntax") {
    import monocle.macros.syntax.lens._

    js2.lens(root.at("two").at("twoa").as[JsString]).getOption shouldEqual Some(JsString("a"))
    encode(js2.lens(root.at("two").at("twoa").as[JsString]).modify { case JsString(s) => JsString(s.toUpperCase) }) shouldEqual """{"one": 1, "two": {"twoa": "A", "twob": ["b1", "b2", "b3"]}}"""
    encode(js2.lens(root.at("two").at("twoa").as[JsString]).set(JsString("c")) shouldEqual """{"one": 1, "two": {"twoa": "c", "twob": ["b1", "b2", "b3"]}}"""
  }*/
}
