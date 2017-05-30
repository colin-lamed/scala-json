package json

import cats.implicits._
import cats.{Alternative, Apply, Foldable, Functor, Monad, MonoidK, SemigroupK, Show, Traverse}
import cats.laws.discipline.{AlternativeTests, ApplyTests, FoldableTests, FunctorTests, MonadTests, MonoidKTests, SemigroupKTests, SerializableTests, TraverseTests}
import org.scalatest.{FunSuite, Matchers}
import json.Json._
import org.scalacheck._
import org.typelevel.discipline.scalatest.Discipline

import org.scalacheck.Arbitrary.arbitrary

class CatsSpec extends FunSuite with Matchers with Discipline with JsonArbitrary {

  val genIntResults: Gen[Result[Int]] = genResult[Int](arbitrary[Int])

  implicit val arbIntResults: Arbitrary[Result[Int]] = Arbitrary(genIntResults)

  val genIntToIntResults: Gen[Result[Int => Int]] = genResult[Int => Int](genAtoA[Int])
  implicit val arbIntToIntResuts: Arbitrary[Result[Int => Int]] = Arbitrary(genIntToIntResults)


  checkAll("Result[Int]", AlternativeTests[Result].alternative[Int, Int, Int])
  checkAll("Alternative[Result]", SerializableTests.serializable(Alternative[λ[α => Result[α]]]))

  checkAll("Show[Result]", SerializableTests.serializable(Show[Result[Int]]))

  checkAll("Result[Int]", MonoidKTests[Result].monoidK[Int])
  checkAll("MonoidK[Result]", SerializableTests.serializable(MonoidK[Result]))

  checkAll("Result[Int]", SemigroupKTests[Result].semigroupK[Int])
  checkAll("SemigroupK[Result]", SerializableTests.serializable(SemigroupK[Result]))

  checkAll("Result[Int]", ApplyTests[Result].apply[Int, Int, Int])
  checkAll("Apply[Result]", SerializableTests.serializable(Apply[Result]))

  checkAll("Result[Int]", FunctorTests[Result].functor[Int, Int, Int])
  checkAll("Functor[Result]", SerializableTests.serializable(Functor[Result]))

  checkAll("Result[Int]", MonadTests[Result].monad[Int, Int, Int])
  checkAll("Monad[Result]", SerializableTests.serializable(Monad[Result]))

  checkAll("Result[Int]", FoldableTests[Result].foldable[Int, Int])
  checkAll("Foldable[Result]", SerializableTests.serializable(Foldable[Result]))

  checkAll("Result[Int]", TraverseTests[Result].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Result]", SerializableTests.serializable(Traverse[Result]))
}
