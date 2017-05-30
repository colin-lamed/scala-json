package json.lens

import monocle.{Iso, Fold, Optional, Prism, Traversal}
import monocle.function.{At, FilterIndex, Index => MIndex, Plated}

import json._
import json.Json._


/** A Read-write lens.
  * In order to support both Read/Write, it exposes Optional - e.g.
  * if parsing/path navigation fails, it returns None. If you require to know
  * why it failed, you can use RLens.
  */
trait RWLens {

  val root: Optional[JsValue, JsValue] = Optional.id

  // TODO tighten to be A <: JsValue rather than just JsValue?
  implicit class RichOptional(o: Optional[JsValue, JsValue]) {
    def at(key: String): Optional[JsValue, JsValue] =
      as[JsObject] composeIso toMapIso composeOptional MIndex.index(key)

    // Note, if we want to be able to remove the value, we need the following
    def at_?(key: String): Optional[JsValue, Option[JsValue]] =
      as[JsObject] composeIso toMapIso composeLens At.at(key)

    def index(i: Int): Optional[JsValue, JsValue] =
      as[JsArray] composeIso toListIso composeOptional MIndex.index(i)

    def as[A](implicit fromJson: FromJson[A], toJson: ToJson[A]): Optional[JsValue, A] =
      o composePrism jsPrism[A]

    // if only fromJson is available, a read-only version
    def as2[A](implicit fromJson: FromJson[A]): Fold[JsValue, A] =
      o composeFold fromJsonFold

    def each: Traversal[JsValue, JsValue] =
      o composeTraversal jsChildrenTraversal

    def filterByIndex(p: Int => Boolean): Traversal[JsValue, JsValue] =
      o composePrism jsPrism[JsArray] composeIso toListIso composeTraversal FilterIndex.filterIndex(p)

    def filterByField(p: String => Boolean): Traversal[JsValue, JsValue] =
      o composePrism jsPrism[JsObject] composeIso toMapIso composeTraversal FilterIndex.filterIndex(p)

    def filter(p: JsValue => Boolean): Fold[JsValue, JsValue] =
      o composeFold Fold.select(p)
  }

  implicit class RichTraversal(o: Traversal[JsValue, JsValue]) {
    def at(key: String): Traversal[JsValue, JsValue] =
      o composePrism jsPrism[JsObject] composeIso toMapIso composeOptional MIndex.index(key)

    // Note, if we want to be able to remove the value, we need the following
    def at2(key: String): Traversal[JsValue, Option[JsValue]] =
      o composePrism jsPrism[JsObject] composeIso toMapIso composeLens At.at(key)

    def index(i: Int): Traversal[JsValue, JsValue] =
      o composePrism jsPrism[JsArray] composeIso toListIso composeOptional MIndex.index(i)

    def as[A](implicit fromJson: FromJson[A], toJson: ToJson[A]): Traversal[JsValue, A] =
      o composePrism jsPrism[A]

    // if only fromJson is available, a read-only version
    def as2[A](implicit fromJson: FromJson[A]): Fold[JsValue, A] =
      o composeFold fromJsonFold

    def each: Traversal[JsValue, JsValue] =
      o composeTraversal jsChildrenTraversal

    def filterByIndex(p: Int => Boolean): Traversal[JsValue, JsValue] =
      as[JsArray] composeIso toListIso composeTraversal FilterIndex.filterIndex(p)

    def filterByField(p: String => Boolean): Traversal[JsValue, JsValue] =
      as[JsObject] composeIso toMapIso composeTraversal FilterIndex.filterIndex(p)

    def filter(p: JsValue => Boolean): Fold[JsValue, JsValue] =
      o composeFold Fold.select(p)
  }

  implicit class RichFold(o: Fold[JsValue, JsValue]) {
    def at(key: String): Fold[JsValue, JsValue] =
      o composePrism jsPrism[JsObject] composeIso toMapIso composeOptional MIndex.index(key)

    // Note, if we want to be able to remove the value, we need the following
    def at2(key: String): Fold[JsValue, Option[JsValue]] =
      o composePrism jsPrism[JsObject] composeIso toMapIso composeLens At.at(key)

    def index(i: Int): Fold[JsValue, JsValue] =
      o composePrism jsPrism[JsArray] composeIso toListIso composeOptional MIndex.index(i)

    def as[A](implicit fromJson: FromJson[A], toJson: ToJson[A]): Fold[JsValue, A] =
      o composePrism jsPrism[A]

    // as or as2 is better? First one requires ToJson, but is an Optional (stronger than Fold)
    // (but not sure where Fold would be more appropriate...)
    def as2[A](implicit fromJson: FromJson[A]): Fold[JsValue, A] =
      o composeFold fromJsonFold

    def each: Fold[JsValue, JsValue] =
      o composeTraversal jsChildrenTraversal

    def filterByIndex(p: Int => Boolean): Fold[JsValue, JsValue] =
      as[JsArray] composeIso toListIso composeTraversal FilterIndex.filterIndex(p)

    def filterByField(p: String => Boolean): Fold[JsValue, JsValue] =
      as[JsObject] composeIso toMapIso composeTraversal FilterIndex.filterIndex(p)

    def filter(p: JsValue => Boolean): Fold[JsValue, JsValue] =
      o composeFold Fold.select(p)
  }

  val toMapIso =
    Iso[JsObject, Map[String, JsValue]](_.value)(JsObject(_))

  val toListIso =
    Iso[JsArray, List[JsValue]](_.value)(JsArray(_))

  def fromJsonFold[A](implicit fromJson: FromJson[A]): Fold[JsValue, A] =
    new Fold[JsValue, A] {
      override def foldMap[M](f: A => M)(v: JsValue)(implicit M: scalaz.Monoid[M]): M =
        fromJson.fromJson1(v).fold(_ => M.zero, f)
    }

  def jsPrism[A](implicit fromJson: FromJson[A], toJson: ToJson[A]): Prism[JsValue, A] =
    Prism[JsValue, A](
      v => fromJson.fromJson1(v).fold(_ => None, a => Some(a)))(
      toJson.toJson1(_))

  /** a Traversal to all values of a JsonObject or JsonList */
  val jsChildrenTraversal: Traversal[JsValue, JsValue] = {

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

    new Traversal[JsValue, JsValue]{
      override def modifyF[F[_]](f: JsValue => F[JsValue])(s: JsValue)(implicit F: scalaz.Applicative[F]): F[JsValue] =
        s match {
          case JsArray(arr)  => F.map(listTraversal.modifyF(f)(arr))(JsArray(_))
          case JsObject(obj) => F.map(mapTraversal.modifyF(f)(obj))(JsObject(_))
          case _             => F.pure(s)
        }
    }
  }

  val jsPlated =
    new Plated[JsValue] {
      override val plate: Traversal[JsValue, JsValue] =
        jsChildrenTraversal
    }
}

object RWLens extends RWLens
