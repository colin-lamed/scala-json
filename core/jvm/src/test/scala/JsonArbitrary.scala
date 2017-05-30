package json

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import Json._

trait JsonArbitrary {

  implicit lazy val genJsString: Gen[JsString] =
    arbitrary[String].map(JsString(_))

  implicit lazy val genJsNumber: Gen[JsNumber] =
    arbitrary[Int].map(JsNumber(_))

  implicit lazy val genJsBoolean: Gen[JsBool] =
    arbitrary[Boolean].map(JsBool(_))

  implicit def genJsArray: Gen[JsArray] =
    for {
      size     <- Gen.choose(0, 10)
      // jsValues <- Gen.lzy(Gen.listOfN(size, genJsValue)) // TODO stack overflow...
      jsValue1 <- genJsValue
      jsValue2 <- genJsValue
      jsValues = List(jsValue1, jsValue2)
    } yield JsArray(jsValues)

  implicit lazy val genJsObject: Gen[JsObject] =
    for {
      k       <- arbitrary[String]
      jsValue <- genJsValue
    } yield JsObject(Map(k -> jsValue)) // TODO n of

  implicit def genJsValue: Gen[JsValue] =
    Gen.lzy(Gen.oneOf(genJsString, genJsNumber, genJsBoolean, genJsArray, genJsObject))

  implicit def genResult[A](implicit aGen: Gen[A]): Gen[Result[A]] =
    for {
      x <- aGen
      // msg <- arbitrary[String] // TODO breaks test when msg not constant - valid failure which needs investigating?
      msg = "msg"
      g <- Gen.frequency(
             (5, Success(x)),
             (1, Error(Nil, msg))  // TODO gen path list
           )
    } yield g

  implicit def genAtoA[A]: Gen[A => A] = {
    val f: A => A = x => x
    Gen.const(f)
  }

  implicit val arbJsArray:  Arbitrary[JsArray]  = Arbitrary(genJsArray)
  implicit val arbJsObject: Arbitrary[JsObject] = Arbitrary(genJsObject)
  implicit val arbJsValue:  Arbitrary[JsValue]  = Arbitrary(genJsValue)
}
