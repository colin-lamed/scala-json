package object json
  extends FromJsonInstances
     with ToJsonInstances

package json {
  trait FromJsonInstances {
    import json.Json._
    import cats._
    import cats.implicits._

    val m = Monad[Result]

    /*implicit val fromJsonForJsValue =
      new FromJson[JsValue] {
        override def fromJson1 = js =>
          m.pure(js)
      }
  */
    implicit def fromJsonForJsValue[T <: JsValue](implicit tag: scala.reflect.ClassTag[T]) =
      new FromJson[T] {
        override def fromJson1 = {
          case tag(t: T) => m.pure(t)
          case v         => typeMismatch("expected " + tag, v)
        }
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
          withString("String")(m.pure(_))
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
          withString("Char"){ t =>
            if (t.length == 1) m.pure(t.head)
            else               fail("Expected a string of length 1")
          }
      }
  }

  trait ToJsonInstances {
    import json.Json._

    /*implicit val toJsonForJsValue =
      new ToJson[JsValue] {
        override def toJson1(js: JsValue) =
          js
      }
  */
    implicit def toJsonForJsValue2[T <: JsValue](implicit tag: scala.reflect.ClassTag[T]) =
      new ToJson[T] {
        override def toJson1(t: T) =
          t
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
}
