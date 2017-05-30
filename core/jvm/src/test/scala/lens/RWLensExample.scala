package json.lens

import org.scalatest.{FunSuite, Matchers}
import json._
import json.Json._
import monocle.{Optional, Traversal}
import cats.implicits._

import RWLens._

class RWLensExample extends FunSuite with Matchers {

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
    val optional: Optional[JsValue, Device] = root.as[Device]

    optional.getOption(js("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""")) shouldEqual
      Some(Device(1, "nameVal", List("one", "two"), true))
  }

  test("JsonPath work with at") {
    val optional: Optional[JsValue, Device] = root.at("key").as[Device]

    optional.getOption(js("""{"key": {"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}}""")) shouldEqual
      Some(Device(1, "nameVal", List("one", "two"), true))
  }

  test("JsonPath work with index") {
    val optional: Optional[JsValue, Device] = root.index(1).as[Device]

    optional.getOption(js(
      """[{"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
         ,{"id": 2, "name": "nameVal2", "values": ["one", "two"], "valid": false}
         ,{"id": 3, "name": "nameVal3", "values": ["one", "two"], "valid": true}
         ]""")) shouldEqual
       Some(Device(2, "nameVal2", List("one", "two"), false))
  }


  test("Lens should work with value") {
    val optional = root.at("one").as[JsNumber]

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    optional.getOption(js2) shouldEqual Some(JsNumber(1))

    val js3: JsValue = optional.modify { case JsNumber(n) => JsNumber(n + 1) }(js2)
    encode(js3) shouldEqual """{"one": 2, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}"""

    val js4 = optional.set(JsNumber(3))(js2)
    encode(js4) shouldEqual """{"one": 3, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}"""
  }

  test("Lens work with nested value") {
    val optional = root.at("two").at("twoa").as[JsString]

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    optional.getOption(js2) shouldEqual Some(JsString("a"))

    val js3: JsValue = optional.modify { case JsString(s) => JsString(s.toUpperCase) }(js2)
    optional.getOption(js3) shouldEqual Some(JsString("A"))

    val js4 = optional.set(JsString("c"))(js2)
    optional.getOption(js4) shouldEqual Some(JsString("c"))
  }

  test("Lens work with arrays") {

    val optional = root.at("two").at("twob").index(1).as[JsString]

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    optional.getOption(js2) shouldEqual Some(JsString("b2"))

    val js3: JsValue = optional.modify { case JsString(s) => JsString(s.toUpperCase) }(js2)
    optional.getOption(js3) shouldEqual Some(JsString("B2"))

    val js4 = optional.set(JsString("c2"))(js2)
    optional.getOption(js4) shouldEqual Some(JsString("c2"))
  }

  test("Lense should work with as JsObject") {

    import cats.implicits._

    case class MyObj(twoa: String, twob: List[String])

    implicit val from =
      new FromJson[MyObj] {
        override def fromJson1 =
          withObject("MyObj") { o =>
            (   o \[String]       "twoa"
            |@| o \[List[String]] "twob"
            ).map(MyObj.apply)
          }
      }

    implicit val to =
      new ToJson[MyObj] {
        override def toJson1(o: MyObj) =
          obj( "twoa" -> toJson(o.twoa)
             , "twob" -> toJson(o.twob)
             )
      }

    val optional = root.at("two").as[MyObj]

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    optional.getOption(js2) shouldEqual Some(MyObj("a", List("b1", "b2", "b3")))

    val js3: JsValue = optional.modify { case MyObj(twoa, twob) => MyObj(twoa + "b", twob ++ List("b4")) }(js2)
    encode(js3) shouldEqual """{"one": 1, "two": {"twoa": "ab", "twob": ["b1", "b2", "b3", "b4"]}}"""

    val js4 = optional.set(MyObj("b", List("c1", "c2", "c3")))(js2)
    encode(js4) shouldEqual """{"one": 1, "two": {"twoa": "b", "twob": ["c1", "c2", "c3"]}}"""
  }

  test("Lens should work with as JsNumber") {
    case class MyObj(n: Int)

    implicit val from =
      new FromJson[MyObj] {
        override def fromJson1 =
          withNumber("number") { n => success(MyObj(n.toInt)) }
      }

    implicit val to =
      new ToJson[MyObj] {
        override def toJson1(o: MyObj) =
          JsNumber(o.n)
      }

    val optional = root.at("one").as[MyObj]

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    optional.getOption(js2) shouldEqual Some(MyObj(1))

    val js3: JsValue = optional.modify { case MyObj(n) => MyObj(n + 1) }(js2)
    encode(js3) shouldEqual """{"one": 2, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}"""

    val js4 = optional.set(MyObj(3))(js2)
    encode(js4) shouldEqual """{"one": 3, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}"""
  }

  test("Lens should work with each") {
    val traversal: Traversal[JsValue, JsValue] = root.at("two").at("twob").each

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    traversal.length(js2) shouldEqual 3
    traversal.getAll(js2) shouldEqual List(JsString("b1"), JsString("b2"), JsString("b3"))

    traversal.headOption(js2) shouldEqual Some(JsString("b1"))
    traversal.lastOption(js2) shouldEqual Some(JsString("b3"))
  }

  test("Lens should work with each and as") {
    case class MyObj(s: String)
    implicit val from =
      new FromJson[MyObj] {
        override def fromJson1 =
          withString("string") { s => success(MyObj(s)) }
      }

    implicit val to =
      new ToJson[MyObj] {
        override def toJson1(o: MyObj) =
          JsString(o.s)
      }

    val traversal: Traversal[JsValue, MyObj] = root.at("two").at("twob").each.as[MyObj]

    val js2 = js("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""")

    traversal.getAll(js2) shouldEqual List(MyObj("b1"), MyObj("b2"), MyObj("b3"))
  }


  /*test("Lens should work with nested value - syntax") {
    import monocle.macros.syntax.lens._

    js2.lens(root.at("two").at("twoa").as[JsString]).getOption shouldEqual Some(JsString("a"))
    encode(js2.lens(root.at("two").at("twoa").as[JsString]).modify { case JsString(s) => JsString(s.toUpperCase) }) shouldEqual """{"one": 1, "two": {"twoa": "A", "twob": ["b1", "b2", "b3"]}}"""
    encode(js2.lens(root.at("two").at("twoa").as[JsString]).set(JsString("c")) shouldEqual """{"one": 1, "two": {"twoa": "c", "twob": ["b1", "b2", "b3"]}}"""
  }*/
}
