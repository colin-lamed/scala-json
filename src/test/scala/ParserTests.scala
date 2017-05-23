package json

import cats.implicits._
import cats.{Alternative, Apply, Functor, Monad, MonoidK, SemigroupK}
import cats.laws.discipline.{AlternativeTests, ApplyTests, FunctorTests, MonadTests, MonoidKTests, SemigroupKTests, SerializableTests}
import org.scalatest.{FunSuite, Matchers}
import json.Json._
import org.scalacheck._
import org.typelevel.discipline.scalatest.Discipline

import org.scalacheck.Arbitrary.arbitrary

class ParserTests extends FunSuite with Matchers with Discipline {

  implicit def parsers[A](implicit aGen: Gen[A]): Gen[Parser[A]] = {
    for {
      x <- aGen
      b <- arbitrary[Boolean]
    } yield {
      if (b) new Parser(Success(x))
      else new Parser(Error(Nil, "msg"))
    }
  }

  val genIntParsers: Gen[Parser[Int]] = parsers[Int](arbitrary[Int])

  implicit val arbIntParsers: Arbitrary[Parser[Int]] = Arbitrary(genIntParsers)

  val genIntToInt: Gen[Int => Int] = {
    val f: Int => Int = x => x
    Gen.const(f)
  }
  val genIntToIntParsers: Gen[Parser[Int => Int]] = parsers[Int => Int](genIntToInt)
  implicit val arbIntToIntResuts: Arbitrary[Parser[Int => Int]] = Arbitrary(genIntToIntParsers)


  checkAll("Parser[Int]", AlternativeTests[Parser].alternative[Int, Int, Int])
  checkAll("Alternative[Parser]", SerializableTests.serializable(Alternative[λ[α => Parser[α]]]))

  checkAll("Parser[Int]", MonoidKTests[Parser].monoidK[Int])
  checkAll("MonoidK[Parser]", SerializableTests.serializable(MonoidK[Parser]))

  checkAll("Parser[Int]", SemigroupKTests[Parser].semigroupK[Int])
  checkAll("SemigroupK[Parser]", SerializableTests.serializable(SemigroupK[Parser]))

  checkAll("Parser[Int]", ApplyTests[Parser].apply[Int, Int, Int])
  checkAll("Apply[Parser]", SerializableTests.serializable(Apply[Parser]))

  checkAll("Parser[Int]", FunctorTests[Parser].functor[Int, Int, Int])
  checkAll("Functor[Parser]", SerializableTests.serializable(Functor[Parser]))

  checkAll("Parser[Int]", MonadTests[Parser].monad[Int, Int, Int])
  checkAll("Monad[Parser]", SerializableTests.serializable(Monad[Parser]))
}
