package json.lens

import monocle.law.discipline.{IsoTests, PrismTests, TraversalTests}
import org.scalatest.{FunSuite, Matchers}
import org.scalacheck.Arbitrary
import org.typelevel.discipline.scalatest.Discipline
import json._
import json.Json._

class MonocleSpec extends FunSuite with Matchers with Discipline with JsonArbitrary {

  implicit val arbResultJsValue: Arbitrary[Result[JsValue]] = Arbitrary(genResult(genJsValue))
  implicit def arbAtoA[A]: Arbitrary[A => A] = Arbitrary(genAtoA)

  implicit val equalForInt:       scalaz.Equal[Int]      = scalaz.Equal.equalA
  implicit val equalForJsArray:   scalaz.Equal[JsArray]  = scalaz.Equal.equalA
  implicit val equalForJsObject:  scalaz.Equal[JsObject] = scalaz.Equal.equalA
  implicit val equalForJsValue:   scalaz.Equal[JsValue]  = scalaz.Equal.equalA
  implicit def equalForMap[A, B]: scalaz.Equal[Map[A,B]] = scalaz.Equal.equalA
  implicit def equalForList[A]:   scalaz.Equal[List[A]]       = scalaz.Equal.equalA
  implicit def equalForResult[A]: scalaz.Equal[Result[A]]       = scalaz.Equal.equalA

  // no FoldTests?

  // RW
  checkAll("RWLens.jsPrism is a Prism", PrismTests(RWLens.jsPrism[Int]))
  checkAll("RWLens.toMapIso is an Iso", IsoTests(RWLens.toMapIso))
  checkAll("RWLens.toListIso is an Iso", IsoTests(RWLens.toListIso))
  checkAll("RWLens.jsChildrenTraversal is a Traversal", TraversalTests(RWLens.jsChildrenTraversal))

  // R
  checkAll("RLens.jsChildrenTraversal is a Traversal", TraversalTests(RLens.jsChildrenTraversal))
}
