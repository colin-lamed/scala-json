package json.lens

import monocle.{Fold, Getter, Traversal}
import monocle.function.{FilterIndex}

import json._
import json.Json._


/** A Read lens.
  * It returns a Result, which contains a description of the error if parsing fails.
  * Note, it does not report navigation path on failure
  */
  // TODO include the navigation path on failure? Requires storing the current path on Success too.
trait RLens {

  val root: Getter[JsValue, Result[JsValue]] = Getter(Success(_))

  // TODO tighten to be A <: JsValue rather than just JsValue?
  implicit class RichGetter(o: Getter[JsValue, Result[JsValue]]) {
    // TODO only if Result[JsValue] above is Result[JsObject]
    def at(key: String): Getter[JsValue, Result[JsValue]] =
      o composeGetter keyGetter(key)

    def at_?(key: String): Getter[JsValue, Result[Option[JsValue]]] =
      o composeGetter optKeyGetter(key)

    def index(i: Int): Getter[JsValue, Result[JsValue]] =
      o composeGetter indexGetter(i)

    def as[A](implicit fromJson: FromJson[A]): Getter[JsValue, Result[A]] =
      o composeGetter jsGetter[A]

    def each: Fold[JsValue, Result[JsValue]] =
      //o composeTraversal jsChildrenTraversal
      o composeFold jsChildrenFold

    def filterByIndex(p: Int => Boolean): Fold[JsValue, Result[JsValue]] =
      as[JsArray] composeFold resultListFold composeTraversal FilterIndex.filterIndex(p)

    def filterByField(p: String => Boolean): Fold[JsValue, Result[JsValue]] =
      as[JsObject] composeFold resultMapFold composeTraversal FilterIndex.filterIndex(p)

    def filter(p: JsValue => Boolean): Fold[JsValue, Result[JsValue]] =
      // TODO or if any result found, no results should be returned - just the errors
      //      suppose it is up to the client to sequence etc as required
      o composeFold Fold.select[Result[JsValue]](_.fold(_ => false, p(_)))
  }

  implicit class RichFold(o: Fold[JsValue, Result[JsValue]]) {
    def at(key: String): Fold[JsValue, Result[JsValue]] =
      o composeGetter keyGetter(key)

    def at_?(key: String): Fold[JsValue, Result[Option[JsValue]]] =
      o composeGetter optKeyGetter(key)

    def index(i: Int): Fold[JsValue, Result[JsValue]] =
      o composeGetter indexGetter(i)

    def as[A](implicit fromJson: FromJson[A]): Fold[JsValue, Result[A]] =
      o composeGetter jsGetter[A]

    def each: Fold[JsValue, Result[JsValue]] =
      // o composeTraversal jsChildrenTraversal
      o composeFold jsChildrenFold

    def filterByIndex(p: Int => Boolean): Fold[JsValue, Result[JsValue]] =
      as[JsArray] composeFold resultListFold composeTraversal FilterIndex.filterIndex(p)

    def filterByField(p: String => Boolean): Fold[JsValue, Result[JsValue]] =
      as[JsObject] composeFold resultMapFold composeTraversal FilterIndex.filterIndex(p)

    def filter(p: JsValue => Boolean): Fold[JsValue, Result[JsValue]] =
      o composeFold Fold.select[Result[JsValue]](_.fold(_ => false, p(_)))
  }

  def keyGetter(key: String): Getter[Result[JsValue], Result[JsValue]] = {
    def get: Result[JsValue] => Result[JsValue] = o => o match {
        case e: Error             => e
        case Success(JsObject(o)) => o.get(key).fold[Result[JsValue]](fail(s"key '$key' not present"))(Success(_))
        case Success(v)           => typeMismatch("Object", v)
      }
    Getter(get)
  }

  def optKeyGetter(key: String): Getter[Result[JsValue], Result[Option[JsValue]]] = {
    def get: Result[JsValue] => Result[Option[JsValue]] = {
      case e: Error             => e
      case Success(JsObject(o)) => Success(o.get(key))
      case Success(v)           => typeMismatch("Object", v)
    }
    Getter(get)
  }

  def indexGetter(i: Int): Getter[Result[JsValue], Result[JsValue]] = {
    def get: Result[JsValue] => Result[JsValue] = {
      case e: Error => e
      case Success(JsArray(a)) => a.lift(i).fold[Result[JsValue]](fail(s"index '$i' not present"))(Success(_))
      case Success(v)          => typeMismatch("Array", v)
    }
    Getter(get)
  }

  val resultListFold: Fold[Result[JsArray], List[Result[JsValue]]] =
    new Fold[Result[JsArray], List[Result[JsValue]]] {
      override def foldMap[M: scalaz.Monoid](f: List[Result[JsValue]] => M)(ra: Result[JsArray]): M =
        ra match {
          case Success(JsArray(a)) => f(a.map(Success(_)))
          case e: Error            => f(List(e))
        }
    }

  val resultMapFold: Fold[Result[JsObject], Map[String, Result[JsValue]]] =
    new Fold[Result[JsObject], Map[String, Result[JsValue]]] {
      override def foldMap[M: scalaz.Monoid](f: Map[String, Result[JsValue]] => M)(ra: Result[JsObject]): M =
        ra match {
          case Success(JsObject(o)) => f(o.map { case (k, v) => (k, Success(v))})
          case e: Error             => f(Map.empty) // ? correct?
        }
    }

  def fromJsonFold[A](implicit fromJson: FromJson[A]): Fold[JsValue, A] =
    new Fold[JsValue, A] {
      override def foldMap[M](f: A => M)(v: JsValue)(implicit M: scalaz.Monoid[M]): M =
        fromJson.fromJson1(v).fold(_ => M.zero, f)
    }

  def jsGetter[A](implicit fromJson: FromJson[A]): Getter[Result[JsValue], Result[A]] = {
    import cats.implicits._
    Getter(_.flatMap(fromJson.fromJson1(_)))
  }


  /** a Traversal to all values of a JsonObject or JsonList */
  val jsChildrenFold: Fold[Result[JsValue], Result[JsValue]] =
    new Fold[Result[JsValue], Result[JsValue]] {
      override def foldMap[M](f: Result[JsValue] => M)(s: Result[JsValue])(implicit M: scalaz.Monoid[M]): M = s match {
        case Success(JsArray(arr))  => // foldMap already defined for List[Monoid]?
                                       arr.foldRight(M.zero)((a, b) => M.append(f(Success(a)), b))
        case Success(JsObject(obj)) => obj.toList.foldRight(M.zero){ case ((k, v), b) => M.append(f(Success(v)), b) }
        case Success(_)       => M.zero
        case e: Error         => f(e)
      }
    }

  /** a Traversal to all values of a JsonObject or JsonList */
  val jsChildrenTraversal: Traversal[Result[JsValue], Result[JsValue]] = {

    def listTraversal[A] =
      new Traversal[List[A], A]{
        def modifyF[F[_]](f: A => F[A])(as: List[A])(implicit F: scalaz.Applicative[F]): F[List[A]] = {
          as.foldRight(F.point(List[A]())){ case (a, facc) =>
            F.apply2(f(a), facc){ (a2, acc) =>
              a2 :: acc
            }
          }
        }
      }

    def mapTraversal[A, B] =
      new Traversal[Map[A, B], B]{
        def modifyF[F[_]](f: B => F[B])(from: Map[A, B])(implicit F: scalaz.Applicative[F]): F[Map[A, B]] = {
          val x = from.toList.foldRight(F.point(List[(A, B)]())){ case ((k, v), facc) =>
            F.apply2(f(v), facc){ (v2, acc) =>
              (k, v2) :: acc
            }
          }
          F.map(x)(Map(_: _*))
        }
      }

    new Traversal[Result[JsValue], Result[JsValue]]{
      override def modifyF[F[_]](f: Result[JsValue] => F[Result[JsValue]])(s: Result[JsValue])(implicit F: scalaz.Applicative[F]): F[Result[JsValue]] =
        s match {
          case Success(JsArray(arr))  => import cats.implicits._
                                         val x: F[List[Result[JsValue]]] = listTraversal.modifyF(f)(arr.map(Success(_)))
                                         val y: F[Result[List[JsValue]]] = F.map(x)(_.sequence)
                                         F.map(y)(_.map(JsArray(_)))
          case Success(JsObject(obj)) => import cats.implicits._
                                         val a: List[(String, Result[JsValue])] = obj.toList.map{ case (k, v) => (k, Success(v)) }
                                         val x: F[Map[String, Result[JsValue]]] = mapTraversal.modifyF(f)(Map(a: _*))
                                         val y: F[Result[Map[String, JsValue]]] = F.map(x){_.sequence}
                                         F.map(y)(_.map(JsObject(_)))
          case _             => F.pure(s)
        }
    }
  }
}

object RLens extends RLens
