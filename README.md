# Json library

## About

A simple json library - not build for performance.

Built upon [Fast Parse](http://www.lihaoyi.com/fastparse/)
and [CATS](https://github.com/typelevel/cats)
and [Monocle](https://github.com/julien-truffaut/Monocle)
and inspired by [Haskell's AESON](https://hackage.haskell.org/package/aeson)

## Adding an SBT dependency

To depend on scala-parser-combinators in SBT, add something like this to your build.sbt:

```
libraryDependencies += "me.github.colinpassiv.play-workflow" %% "scala-json" % "0.0.1"
resolvers += "jitpack" at "https://jitpack.io"
```
## Example

### Parsing from JSON

Create instances of `FromJson` for your model.

Using either for-comprehension:

```scala
import cats.implicits._
import Json._

implicit val fromJsonForTuple1 =
  new FromJson[(Int, String, List[String], Boolean)] {
    override def fromJson1 =
      withObject("Tuple of 4") { o =>
        for {
          id     <- o \[Int]          "id"
          name   <- o \[String]       "name"
          values <- o \[List[String]] "values"
          valid  <- o \[Boolean]      "valid"
        } yield (id, name, values, valid)
      }
  }
```

Or applicative syntax:

```scala
import cats.implicits._
import Json._

case class Device(id: Int, name: String, values: List[String], valid: Boolean)

implicit val fromJsonForDevice =
  new FromJson[Device] {
    override def fromJson1 =
      withObject("Device object") { o =>
        (   o \[Int]          "id"
        |@| o \[String]       "name"
        |@| o \[List[String]] "values"
        |@| o \[Boolean]      "valid"
        ).map(Device.apply)
      }
  }
```

The json can then be parsed with the FromJson implicit being in scope

```scala
val optionModel: Option[Device] = decode("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""")

val eitherModel: Either[String,Device] = decodeEither("""{"id": 1, "name": "nameVal", "values": ["one", "two"], "valid": true}""")
```

### Parsing to JSON

Create instances of ToJson for your model.

```scala
implicit val toJsonForDevice =
  new ToJson[Device] {
    override def toJson1(d: Device) =
      obj( "id"     -> toJson(d.id)
         , "name"   -> toJson(d.name)
         , "values" -> toJson(d.values)
         , "valid"  -> toJson(d.valid)
         )
  }
```

Then can create JSON

```scala
val device = Device(1, "nameVal", List("one", "two"), true)
val js: JsValue = toJson(device)
val str: String = encode(device)
```


## Optics

There are two sets of optics.

### Read/Write

API exposes Optional for get/modify/set.
Start with ```RWLens.root```, then navigate through json with
- `at("key")` for JsObject
- `index(i)` for JsArray
- `as[Target]` to parse as Target - requires `FromJson[Target]` and `ToJson[Target]`

This produces an `Optional[JsValue, Target]`. Json can now be accessed or modified with Monocle API. E.g.
- `getOption` - returns None if navigation/parse fails
- `modify` - applies a function to the value under focus. Returns an updated JsValue, or the original JsValue if navigation/parse fails
  `modifyOption` - same as modify, but returns None if the navigation/parse fails
- `set` - replace the value under focus with a new value. Returns an updated JsValue, or the original JsValue if navigation/parse fails
- `setOption` - same as set, but returns None if the navigation/parse fails

```scala
val optional: Optional[JsValue, JsString] = RWLens.root.at("two").at("twob").index(1).as[JsString]

val s = """{"one": 3, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}"""

// getOption - returns None if navigation or parse fails
val optJsString: Option[JsString] =
  for {
    json     <- JsonParser.parse(s).toOption
    jsstring <- optional.getOption(js2)
  } yield jsstring
// JsString("b2")

// modify - returns
val js2: JsValue = optional.modify { case JsString(s) => JsString(s.toUpper) }(json)
encode(js2)
// """{"one": 3, "two": {"twoa": "a", "twob": ["b1", "B2", "b3"]}}"""

val js3 = optional.set(JsString(3))(json)
encode(js3)
// """{"one": 3, "two": {"twoa": "a", "twob": ["b1", "3", "b3"]}}"""
```

we can also get a traversal
- `each` navigates through all children (for JsArray and JsObject values)
- `filterByIndex(p: Int => Boolean)` navigates through JsArray children matching predicate
- `filterByField(p: String => Boolean)` navigates through JsObject children matching predicate
- `filter(p: JsValue => Boolean)` navigates through children mathcing predicate. Note this returns a Read-only Fold rather than a Traversal.

```scala
val traversal: Traversal[JsValue, JsValue] = RWLens.root.at("two").at("twob").each

val json = JsonParser.parse("""{"one": 1, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}""").toOption.getOrElse(sys.error("Failed to parse json"))
traversal.length(json)
// 3
traversal.getAll(json)
// List(JsString("b1"), JsString("b2"), JsString("b3"))
```

### Read only

Optic is created from `RLens.root`
The difference with `RWLens.root` is that instead of an `Optional[JsValue, Target]` we get a `Getter[JsValue, Result[Target]]` - i.e. a read-only API but it will report the cause of any parse errors.

```scala
val getter: Getter[JsValue, Result[JsString]] = RLens.root.at("two").at("twob").index(1).as[JsString]

val s = """{"one": 3, "two": {"twoa": "a", "twob": ["b1", "b2", "b3"]}}"""

// getOption - returns None if navigation or parse fails
val optJsString: Option[JsString] =
  for {
    json     <- JsonParser.parse(s).toOption
    jsstring <- getter.getOption(js2)
  } yield jsstring
// Success(JsString("b2"))

// failure
val s = """{"one": 3, "two": {"twoa": "a", "twob": [1, 2, 3]}}"""

// getOption - returns None if navigation or parse fails
val optJsString: Option[JsString] =
  for {
    json     <- JsonParser.parse(s).toOption
    jsstring <- getter.getOption(js2)
  } yield jsstring
// Error(List(), "Expected expected String, encountered Number")
```

Note the error path is relative to the parse, it does not include the path navigated to reach the parse point.

Similarly, `each` returns a read-only `Fold` rather than a read-write `Traversal`.
