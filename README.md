# Json library

## About

A simple json library - not build for performance.

Built upon [Fast Parse](http://www.lihaoyi.com/fastparse/)
and [CATS](https://github.com/typelevel/cats)
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
