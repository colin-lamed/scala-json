package json

import org.scalatest._

import cats.implicits._

import Json._

class NestedModelJsonExample extends FlatSpec with Matchers {

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

  "Nested Model Json" should "parse model" in {
    decode[Model2]("""
      {
        "values":
          [ {"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
          , {"id": 2, "name": "nameVal2", "values": ["three", "four"], "valid": false}
          ],
        "value2":
          {"id": 3, "name": "nameVal3", "values": ["five", "six"], "valid": true}
      }""") should be (
      Success(Model2(
        List(
          Model1(1, "nameVal1", List("one", "two"), true),
          Model1(2, "nameVal2", List("three", "four"), false)),
        Model1(3, "nameVal3", List("five", "six"), true)))
    )
  }

  it should "report failure for objects" in {
    val error = decode[Model2]("""
      {
        "values":
          [ {"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
          , {"id": 2, "name": "nameVal2", "values": ["three", "four"], "valid": false}
          ],
        "value2":
          {"id": 3, "name": "nameVal3", "values": ["five", "six"], "valid": "true"}
      }""")
    error match {
      case e@Error(p, msg) => p should be (List(Key("value2"), Key("valid")))
                              msg should be ("expected Boolean, encountered String")
                              e .formatError should be ("Error in $.value2.valid: expected Boolean, encountered String")
      case _ => fail("decode should have returned Error")
    }
  }

  it should "report failure for arrays" in {
    val error = decode[Model2]("""
      {
        "values":
          [ {"id": 1, "name": "nameVal1", "values": ["one", "two"], "valid": true}
          , {"id": "2", "name": "nameVal2", "values": ["three", "four"], "valid": false}
          ],
        "value2":
          {"id": 3, "name": "nameVal3", "values": ["five", "six"], "valid": true}
      }""")
    error match {
      case e@Error(p, msg) => p should be (List(Key("values"), Index(1), Key("id")))
                              msg should be ("expected Number, encountered String")
                              e .formatError should be ("Error in $.values[1].id: expected Number, encountered String")
      case _ => fail("decode should have returned Error")
    }
  }
}
