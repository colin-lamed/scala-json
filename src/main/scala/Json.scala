package json

import cats._
import cats.implicits._
import simulacrum.typeclass

import scala.language.implicitConversions

sealed trait JsValue
case class JsObject(value: Map[String,JsValue]) extends JsValue
case class JsArray(value: List[JsValue])        extends JsValue
case class JsString(value: String)              extends JsValue
case class JsNumber(value: BigDecimal)          extends JsValue
case class JsBool(value: Boolean)               extends JsValue
case object JsNull                              extends JsValue

object Json {

  sealed trait JsonPathElement
  /** Json path element of a key into an object*/
  case class Key(key: String) extends JsonPathElement
  /** Json path element of an index into an array*/
  case class Index(i: Int) extends JsonPathElement

  type JsonPath = List[JsonPathElement]

  sealed trait Result[+A]
  case class Error(path: JsonPath, msg: String) extends Result[Nothing] {
    /** Annotate an error message with a <http://goessner.net/articles/JsonPath/ JSONPath> error location.*/
    def formatError: String = {
      def escape(key: String) = {
        key.flatMap {
          case '\'' => "\\'"
          case '\\' => "\\\\"
          case c    => c.toString
        }
      }
      def formatKey(key: String) = {
        val isIdentifier = key.headOption.map(h => h.isLetter && key.tail.forall(_.isLetterOrDigit)).getOrElse(false)
        if (isIdentifier) s".$key"
        else              s"[${escape(key)}]"
      }
      def format(pfx: String, path: JsonPath): String = path match {
        case List()             => pfx
        case (Index(i) :: rest) => format(s"$pfx[$i]"          , rest)
        case (Key(k)   :: rest) => format(s"$pfx${formatKey(k)}", rest)
      }
      s"Error in ${format("$", path)}: $msg"
    }
  }

  case class Success[A](a: A) extends Result[A]


  // MonadFail, MonadPlus covered?
  implicit val catsInstancesForResult: MonadError[Result, String] with Traverse[Result] with Alternative[Result] =
    new MonadError[Result, String] with Traverse[Result] with Alternative[Result] {
      // for Applicative
      override def pure[A](a: A): Success[A] =
        Success(a)

      // for Applicative
      override def ap[A, B](ff: Result[A => B])(fa: Result[A]): Result[B] =
        (ff, fa) match {
          case (Success(f)  , Success(a)  ) => Success(f(a))
          case (_           , e@Error(_,_)) => e
          case (e@Error(_,_), _           ) => e
        }

      // for ApplicativeError
      override def handleErrorWith[A](fa: Result[A])(f: String => Result[A]): Result[A] =
        fa match {
          case Error(_, msg)  => f(msg)
          case s @ Success(_) => s
        }

      // for ApplicativeError
      override def raiseError[A](e: String): Result[A] =
        Error(Nil, e)

      // for FlatMap
      override def flatMap[A, B](fa: Result[A])(f: A => Result[B]): Result[B] =
        fa match {
          case Success(a) => f(a)
          case e@Error(_,_) => e
        }

      override def map[A, B](fa: Result[A])(f: A => B): Result[B] =
        fa match {
          case Success(a) => Success(f(a))
          case e@Error(_, _) => e
        }

      // for FlatMap
      @annotation.tailrec
      override def tailRecM[A, B](a: A)(f: A => Result[Either[A, B]]): Result[B] =
        f(a) match {
          case e @ Error(_, _) => e
          case Success(e) =>
            e match {
              case Left(a1) => tailRecM(a1)(f)
              case Right(b) => pure(b)
            }
        }

      // for Traverse
      override def traverse[G[_], A, B](fa: Result[A])(f: A => G[B])(implicit G: cats.Applicative[G]): G[Result[B]] =
        fa match {
          case e@ Error(_, _) => G.pure(e)
          case Success(a)     => G.map(f(a))(Success(_))
        }

      // for Foldable
      override def foldLeft[A, B](fa: json.Json.Result[A], b: B)(f: (B, A) => B): B =
         fa match {
           case Error(_, _) => b
           case Success(a)  => f(b, a)
         }

      // for Foldable
      override def foldRight[A, B](fa: json.Json.Result[A], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
         fa match {
           case Error(_, _) => lb
           case Success(a)  => f(a, lb)
         }

      // for SemigroupK
      override def combineK[A](x: Result[A], y: Result[A]): Result[A] =
        (x, y) match {
          case (Error(_, _), Error(_, "mempty")) => x
          case (Error(_, _), _                 ) => y
          case (Success(_) , _                 ) => x
        }

      // for MonoidK
      override def empty[A]: Result[A] =
        Error(Nil, "mempty")
    }

    implicit def eqForResult[A](implicit F: Eq[A]): Eq[Result[A]] =
      new Eq[Result[A]] {
        override def eqv(x: Result[A], y: Result[A]): Boolean =
          (x, y) match {
            case (Error(path1, msg1), Error(path2, msg2)) => path1 == path2 && msg1 == msg2
            case (Success(a1)       , Success(a2)       ) => F.eqv(a1, a2)
            case (_                 , _                 ) => false
          }
      }

    implicit def showForResult[A]: Show[Result[A]] = new Show[Result[A]] {
      def show(f: Result[A]): String = f.toString
    }

  final class Parser[A](result: => Result[A]) {
    def runParser =
      result

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      runParser match {
        case Error(path, msg) => new Parser(Error(path, msg))
        case Success(a)       => f(a)
      }
  }

  object Parser {
    def success[A](a: A) =
      new Parser(Success(a))

    def failure[A](msg: String) =
      new Parser[A](Error(List(), msg))
  }

  implicit def monadForParser: MonadError[Parser, String] with Monad[Parser] with Alternative[Parser] =
    new MonadError[Parser, String] with Monad[Parser] with Alternative[Parser] {
      // for Applicative
      override def pure[A](a: A): Parser[A] =
        Parser.success(a)

      // for Applicative
      override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = {
        val result = (ff.runParser, fa.runParser) match {
          case (Success(f)  , Success(a)  ) => Success(f(a))
          case (_           , e@Error(_,_)) => e
          case (e@Error(_,_), _           ) => e
        }
        new Parser(result)
      }

      // for ApplicativeError
      override def handleErrorWith[A](fa: Parser[A])(f: String => Parser[A]): Parser[A] =
        fa.runParser match {
          case Error(_, msg) => f(msg)
          case Success(a)    => pure(a)
        }

      // for ApplicativeError
      override def raiseError[A](e: String): Parser[A] =
        Parser.failure(e)

      // for FlatMap
      override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
        fa.flatMap(f)

      // for FlatMap
      @annotation.tailrec
      override def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] =
         f(a).runParser match {
           case e @ Error(_, _) => new Parser(e)
           case Success(e)      => e match {
             case Left(a1)  => tailRecM(a1)(f)
             case Right(b) => pure(b)
           }
         }

      //  for MonoidK
      def empty[A]: json.Json.Parser[A] =
        fail("mempty")

      // for SemigroupK
      def combineK[A](x: json.Json.Parser[A], y: json.Json.Parser[A]): json.Json.Parser[A] = {
        (x.runParser, y.runParser) match {
          case (Error(_, _), Error(_, "mempty")) => x
          case (Error(_, _), _                 ) => y
          case (Success(_),  _                 ) => x
        }
      }
    }

    implicit def eqForParser[A](implicit F: Eq[A]): Eq[Parser[A]] =
      new Eq[Parser[A]] {
        val e = implicitly[Eq[Result[A]]]
        override def eqv(x: Parser[A], y: Parser[A]): Boolean =
          e.eqv(x.runParser, y.runParser)
      }

  @typeclass trait FromJson[A] {
    def fromJson1: JsValue => Parser[A]
  }


  @typeclass trait ToJson[A] {
    def toJson1(a: A): JsValue
  }

  // --- helpers for creating FromJSON -----------------------------------------

  def fail[A](s: String): Parser[A] = {
    val me = implicitly[MonadError[Parser, String]]
    me.raiseError[A](s)
  }

  def typeMismatch[A](expected: String, actual: JsValue): Parser[A] = {
    val name = actual match {
      case _: JsObject => "Object"
      case _: JsArray  => "Array"
      case _: JsString => "String"
      case _: JsNumber => "Number"
      case _: JsBool   => "Boolean"
      case    JsNull   => "Null"
    }
    fail(s"expected $expected, encountered $name")
  }


  def withObject[A](expected: String)(f: Map[String, JsValue] => Parser[A]): JsValue => Parser[A] = {
    case JsObject(o) => f(o)
    case v           => typeMismatch(expected, v)
  }

  def withText[A](expected: String)(f: String => Parser[A]): JsValue => Parser[A] = {
    case JsString(s) => f(s)
    case v           => typeMismatch(expected, v)
  }

  def withArray[A](expected: String)(f: List[JsValue] => Parser[A]): JsValue => Parser[A] = {
    case JsArray(a) => f(a)
    case v          => typeMismatch(expected, v)
  }

  def withNumber[A](expected: String)(f: BigDecimal => Parser[A]): JsValue => Parser[A] = {
    case JsNumber(a) => f(a)
    case v           => typeMismatch(expected, v)
  }

  def withBoolean[A](expected: String)(f: Boolean => Parser[A]): JsValue => Parser[A] = {
    case JsBool(a) => f(a)
    case v         => typeMismatch(expected, v)
  }


  implicit class RichParser[A](p: Parser[A]) {
    def addPath(jsonPathElement: JsonPathElement): Parser[A] = {
      val result = p.runParser match {
        case Error(path, msg) => Error(jsonPathElement :: path, msg)
        case Success(a)       => Success(a)
      }
      new Parser[A](result)
    }

    def updateFailure(f: String => String): Parser[A] = {
      val result = p.runParser match {
        case Error(path, msg) => Error(path, f(msg))
        case Success(a)       => Success(a)
      }
      new Parser[A](result)
    }
  }

  implicit class RichJsObject(o: Map[String, JsValue]) {
    def \[A](key: String)(implicit ev: FromJson[A]): Parser[A] = {
      o.get(key) match {
        case Some(v) => ev.fromJson1(v) addPath Key(key)
        case None    => fail(s"key '$key' not present")
      }
    }
    def \?[A](key: String)(implicit ev: FromJson[A]): Parser[Option[A]] = {
      o.get(key) match {
        case Some(v) => (ev.fromJson1(v).map(Some(_)): Parser[Option[A]]) addPath Key(key)
        case None    => val m = implicitly[Monad[Parser]]
                        m.pure(None)
      }
    }
  }

  // --- helpers for creating ToJSON -------------------------------------------

  def obj(values: (String, JsValue)*): JsObject =
    JsObject(Map(values: _*))

  def arr(values: JsValue*): JsArray =
    JsArray(values.toList)

  // -- The API ----------------------------------------------------------------

  def toJson[A](a: A)(implicit ev: ToJson[A]): JsValue =
    ev.toJson1(a)

  def fromJson[A](v: JsValue)(implicit ev: FromJson[A]): Parser[A] =
    ev.fromJson1(v)

  def parseJson[A](v: JsValue)(implicit ev: FromJson[A]): Result[A] =
    fromJson(v).runParser

  def parseJsonEither[A](v: JsValue)(implicit ev: FromJson[A]): Either[String, A] =
    parseJson(v) match {
      case Error(_, msg) => Left(msg)
      case Success(a)    => Right(a)
    }

  def parseJsonOption[A](v: JsValue)(implicit ev: FromJson[A]): Option[A] =
    parseJsonEither(v).fold(_ => None, Some(_))

  def decode[A](str: String)(implicit ev: FromJson[A]): Result[A] =
    JsonParser.parse(str) match {
      case Left(msg)      => Error(Nil, msg)
      case Right(jsValue) => parseJson(jsValue)
    }

  def decodeEither[A](str: String)(implicit ev: FromJson[A]): Either[String, A] =
    decode(str) match {
      case Error(_, msg) => Left(msg)
      case Success(a)    => Right(a)
    }

  def decodeOption[A](str: String)(implicit ev: FromJson[A]): Option[A] =
    decodeEither(str).fold(_ => None, Some(_))

  def encodeJson(v: JsValue): String =
    v match {
      case JsObject(value: Map[String,JsValue]) => value.map { case (k, v) => encodeJson(JsString(k)) + ": " + encodeJson(v) }.mkString("{", ", ", "}")
      case JsArray(value: List[JsValue])        => value.map(encodeJson(_)).mkString("[", ", ", "]")
      case JsString(value: String)              => "\"" + value + "\""
      case JsNumber(value: BigDecimal)          => value.toString
      case JsBool(value: Boolean)               => value.toString
      case JsNull                               => "null"
    }

  def encode[A](a: A)(implicit ev: ToJson[A]): String =
    encodeJson(toJson(a))
}

trait FromJsonInstances {
  import Json._

  val m = implicitly[Monad[Parser]]

  implicit val fromJsonForJsValue =
    new FromJson[JsValue] {
      override def fromJson1 = js =>
        m.pure(js)
    }

  implicit def fromJsonForOption[A](implicit ev: FromJson[A]) =
    new FromJson[Option[A]] {
      override def fromJson1 = {
        case JsNull => m.pure(None)
        case x      => fromJson[A](x).map(Some(_))
      }
    }

  implicit def fromJsonForList[A](implicit ev: FromJson[A]) =
    new FromJson[List[A]] {
      override def fromJson1 =
        withArray("Array")(
          _.zipWithIndex.traverse { case (v,i) =>
            fromJson[A](v) addPath Index(i)
          }
        )
    }

  implicit val fromJsonForString =
    new FromJson[String] {
      override def fromJson1 =
        withText("String")(m.pure(_))
    }

  implicit val fromJsonForBigDecimal =
    new FromJson[BigDecimal] {
      override def fromJson1 =
        withNumber("Number")(m.pure(_))
    }

  implicit val fromJsonForInt =
    new FromJson[Int] {
      override def fromJson1 =
        withNumber("Number"){ n => m.pure(n.intValue) }
    }

  implicit val fromJsonForShort =
    new FromJson[Short] {
      override def fromJson1 =
        withNumber("Number"){ n => m.pure(n.shortValue) }
    }

  implicit val fromJsonForLong =
    new FromJson[Long] {
      override def fromJson1 =
        withNumber("Number"){ n => m.pure(n.longValue) }
    }

  implicit val fromJsonForFloat =
    new FromJson[Float] {
      override def fromJson1 =
        withNumber("Number"){ n => m.pure(n.floatValue) }
    }

  implicit val fromJsonForDouble =
    new FromJson[Double] {
      override def fromJson1 =
        withNumber("Number"){ n => m.pure(n.doubleValue) }
    }


  implicit val fromJsonForBoolean =
    new FromJson[Boolean] {
      override def fromJson1 =
        withBoolean("Boolean")(m.pure(_))
    }

  implicit val fromJsonForChar =
    new FromJson[Char] {
      override def fromJson1 =
        withText("Char"){ t =>
          if (t.length == 1) m.pure(t.head)
          else               fail("Expected a string of length 1")
        }
    }
}

trait ToJsonInstances {
  import Json._

  implicit val toJsonForJsValue =
    new ToJson[JsValue] {
      override def toJson1(js: JsValue) =
        js
    }

  // what if key shouldn't appear at all?
  implicit def toJsonForOption[A](implicit ev: ToJson[A]) =
    new ToJson[Option[A]] {
      override def toJson1(l: Option[A]) =
        l match {
          case Some(a) => toJson(a)
          case None    => JsNull
        }
    }

  implicit def toJsonForList[A](implicit ev: ToJson[A]) =
    new ToJson[List[A]] {
      override def toJson1(l: List[A]) =
        JsArray(l.map(toJson(_)))
    }

  implicit val toJsonForString =
    new ToJson[String] {
      override def toJson1(s: String) =
        JsString(s)
    }

  implicit val toJsonForBigDecimal =
    new ToJson[BigDecimal] {
      override def toJson1(bd: BigDecimal) =
        JsNumber(bd)
    }

  implicit val toJsonForInt =
    new ToJson[Int] {
      override def toJson1(i: Int) =
        JsNumber(i)
    }

  implicit val toJsonForShort =
    new ToJson[Short] {
      override def toJson1(s: Short) =
        JsNumber(s)
    }

  implicit val toJsonForLong =
    new ToJson[Long] {
      override def toJson1(l: Long) =
        JsNumber(l)
    }

  implicit val toJsonForFloat =
    new ToJson[Float] {
      override def toJson1(f: Float) =
        JsNumber(f)
    }

  implicit val toJsonForDouble =
    new ToJson[Double] {
      override def toJson1(d: Double) =
        JsNumber(d)
    }

  implicit val toJsonForBoolean =
    new ToJson[Boolean] {
      override def toJson1(b: Boolean) =
        JsBool(b)
    }

  implicit val toJsonForChar =
    new ToJson[Char] {
      override def toJson1(c: Char) =
        JsString(c.toString)
    }
}
