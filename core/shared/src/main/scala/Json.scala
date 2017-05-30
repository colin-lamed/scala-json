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

object JsValue {
  implicit val eqForJsValue: Eq[JsValue] =
    new Eq[JsValue] {
      override def eqv(x: JsValue, y: JsValue): Boolean =
        x == y
    }
  }


// TODO move all cats dependencies into it's own trait?
object Json {

  sealed trait JsonPathElement
  /** Json path element of a key into an object*/
  case class Key(key: String) extends JsonPathElement
  /** Json path element of an index into an array*/
  case class Index(i: Int) extends JsonPathElement

  type JsonPath = List[JsonPathElement]

  sealed trait Result[+A] {
    def fold[B](fe: String => B, fs: A => B): B =
      this match {
        case Error(path, msg) => fe(msg)
        case Success(a)       => fs(a)
      }
  }

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

  @typeclass trait FromJson[A] {
    def fromJson1: JsValue => Result[A]
  }


  @typeclass trait ToJson[A] {
    def toJson1(a: A): JsValue
  }

  // --- helpers for creating FromJSON -----------------------------------------

  // TODO or move to Result.fail
  def fail[A](s: String): Result[A] =
    MonadError[Result, String].raiseError[A](s)

  def success[A](a: A): Result[A] =
    Monad[Result].pure(a)

  def typeMismatch[A](expected: String, actual: JsValue): Result[A] = {
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


  def withObject[A](expected: String)(f: Map[String, JsValue] => Result[A]): JsValue => Result[A] = {
    case JsObject(o) => f(o)
    case v           => typeMismatch(expected, v)
  }

  def withString[A](expected: String)(f: String => Result[A]): JsValue => Result[A] = {
    case JsString(s) => f(s)
    case v           => typeMismatch(expected, v)
  }

  def withArray[A](expected: String)(f: List[JsValue] => Result[A]): JsValue => Result[A] = {
    case JsArray(a) => f(a)
    case v          => typeMismatch(expected, v)
  }

  def withNumber[A](expected: String)(f: BigDecimal => Result[A]): JsValue => Result[A] = {
    case JsNumber(a) => f(a)
    case v           => typeMismatch(expected, v)
  }

  def withBoolean[A](expected: String)(f: Boolean => Result[A]): JsValue => Result[A] = {
    case JsBool(a) => f(a)
    case v         => typeMismatch(expected, v)
  }


  implicit class RichResult[A](r: Result[A]) {
    def addPath(jsonPathElement: JsonPathElement): Result[A] =
      r match {
        case Error(path, msg) => Error(jsonPathElement :: path, msg)
        case Success(a)       => Success(a)
      }

    def updateFailure(f: String => String): Result[A] =
      r match {
        case Error(path, msg) => Error(path, f(msg))
        case Success(a)       => Success(a)
      }
  }

  implicit class RichJsObject(o: Map[String, JsValue]) {
    def \[A](key: String)(implicit ev: FromJson[A]): Result[A] = {
      o.get(key) match {
        case Some(v) => ev.fromJson1(v) addPath Key(key)
        case None    => fail(s"key '$key' not present")
      }
    }
    def \?[A](key: String)(implicit ev: FromJson[A]): Result[Option[A]] = {
      o.get(key) match {
        case Some(v) => (ev.fromJson1(v).map(Some(_)): Result[Option[A]]) addPath Key(key)
        case None    => Monad[Result].pure(None)
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

  def fromJson[A](v: JsValue)(implicit ev: FromJson[A]): Result[A] =
    ev.fromJson1(v)

  def parseJson[A](v: JsValue)(implicit ev: FromJson[A]): Result[A] =
    fromJson[A](v)

  def parseJsonEither[A](v: JsValue)(implicit ev: FromJson[A]): Either[String, A] =
    parseJson[A](v) match {
      case Error(_, msg) => Left(msg)
      case Success(a)    => Right(a)
    }

  def parseJsonOption[A](v: JsValue)(implicit ev: FromJson[A]): Option[A] =
    parseJsonEither[A](v).fold(_ => None, Some(_))

  def decode[A](str: String)(implicit ev: FromJson[A]): Result[A] =
    JsonParser.parse(str) match {
      case Left(msg)      => Error(Nil, msg)
      case Right(jsValue) => parseJson[A](jsValue)
    }

  def decodeEither[A](str: String)(implicit ev: FromJson[A]): Either[String, A] =
    decode[A](str) match {
      case Error(_, msg) => Left(msg)
      case Success(a)    => Right(a)
    }

  def decodeOption[A](str: String)(implicit ev: FromJson[A]): Option[A] =
    decodeEither[A](str).fold(_ => None, Some(_))

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
