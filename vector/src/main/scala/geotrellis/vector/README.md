#geotrellis.vector

>"Raster is faster but vector is correcter."
— Somebody

##Features and Geometries

In addition to working with raster data, Geotrellis provides a number of facilities for the creation, representation, and modification of vector data.
The data types central to this functionality (`geotrellis.vector.Feature` and `geotrellis.vector.Geometry`) correspond - and not by accident - to certain objects found in [the GeoJson spec](http://geojson.org/geojson-spec.html).
`Feature`s correspond to GeoJSON objects with type `Feature`.
`Geometry`s correspond to GeoJSON objects with type `Point`, `MultiPoint`, `LineString`, `MultiLineString`, `Polygon`, `MultiPolygon`, or `GeometryCollection`.

##Geometries

GeoTrellis uses the JTS library as the basis for computational geometry algorithms, but provides its own Geometry type hierarchy to adapt the JTS API to one more convenient to Scala developers and coherent with GeoTrellis design goals.
Notable differences include:
   * The GeoTrellis Geometry hierarchy models the GeoJSON specification,
     instead of the slightly more complex Simple Features Specification used in
     JTS.

   * Where JTS algorithms often return the bare `Geometry` type, GeoTrellis
     tries to specify more concrete types where they are statically known.  For
     example, applying an affine transformation always produces the same type
     of Geometry that the type is being applied to, and this is reflected in
     GeoTrellis vector.

   * In the case of algorithms that may produce a few different types of
     Geometry, but not all types, the GeometryResult type works with Scala's
     exhaustiveness checker to detect when calling code does not account for all
     possible results. For example, the intersection between a Line and a Point
     can only be a Point, or the empty geometry, so this operation returns a
     PointOrNoResult.

   * Extractors are provided to allow pattern matching over the contents of
     Geometries.  For example, you can extract the coordinates of a Point with:
     `val Point(x,y) = point` .

The base `Geometry` class can be found in `Geometry.scala`. Concrete geometries include:
+ `geotrellis.vector.Point`
+ `geotrellis.vector.MultiPoint`
+ `geotrellis.vector.Line`
+ `geotrellis.vector.MultiLine`
+ `geotrellis.vector.Polygon`
+ `geotrellis.vector.MultiPolygon`
+ `geotrellis.vector.GeometryCollection`

Working with these geometries is a relatively straightforward affair. Let's take a look:
```scala
import geotrellis.vector._
/*
 * First, let's create a Point. Then, we'll use its intersection method.
 * Note: we are also using intersection's alias '&'.
 */
val myPoint = Point(1.0, 1.1) // Create a point
val selfIntersection = myPoint intersection Point(1.0, 1.1) // Intersection method
val nonIntersection = myPoint & Point(200, 300) // Intersection alias
```
At this point, the values `selfIntersection` and `nonIntersection` are `GeometryResult` containers.
Many geometry operations can only produce a small subset of geometry types - in this case, an intersection betwen two points can only be a Point or no geometry at all.
In these cases GeoTrellis specifies a return type that reflects that subset of Geometry types.
In this example the result is a `PointOrNoResult`.
The benefit over simply returning the general Geometry type is that Scala's compiler provides exhaustiveness checking for patterns over the GeometryResult types:

```scala
> def unwrapPoint(res: PointOrNoResult): Option[Point] = res match {
    case PointResult(point) => Some(point)
  }
<console>:10: warning: match may not be exhaustive.
It would fail on the following input: NoResult
```

If we add the NoResult case then there is no such warning:

```scala
def unwrapPoint(res: PointOrNoResult): Option[Point] =
  res match {
    case PointResult(point) => Some(point)
    case NoResult => None
  }
// Et voila:
assert(unwrapPoint(selfIntersection) == Some(myPoint))  // Either some point
assert(unwrapPoint(nonIntersection) == None)  // Or nothing at all
```

Beyond the methods which come with any `Geometry` object there are implicits in many geotrellis modules which will extend Geometry capabilities. For instance, after importing `geotrellis.vector.io.json._`, it becomes possible to call the `toGeoJson` method on any `Geometry`:
```scala
import geotrellis.vector.io.json._
assert(Point(1,1).toGeoJson == """{"type":"Point","coordinates":[1.0,1.0]}""")
```
If you need to move from a geometry to a serialized representation or vice-versa, take a look at the `io` directory's contents. This naming convention for input and output is common throughout Geotrellis. So if you're trying to get spatial representations in or out of your program, spend some time seeing if the problem has already been solved.

The following packages extend `Geometry` capabilities:
- [geotrellis.vector.io.json](io/json/)
- [geotrellis.vector.io.WKT](io/WKT/)
- [geotrellis.vector.io.WKB](io/WKB/)
- [geotrellis.vector.affine](affine/)
- [geotrellis.vector.reproject](reproject/)

### JTS Interoperability
Every GeoTrellis Geometry wraps a JTS Geometry.
When a GeoTrellis Geometry is constructed from a JTS Geometry, it references the original object rather than making a defensive copy.
JTS Geometry objects do expose some in-place mutating operations, so GeoTrellis Geometry objects are also mutable.
However, GeoTrellis ensures that appropriate copies are made when using mutating operations, so it is usually safe to treat Geometry as immutable.

In some cases JTS may provide functionality that has not been exposed in the GeoTrellis API.
In this case you can access the JTS Geometry underlying a GeoTrellis Geometry via the .unsafeGeom method. Subtypes of GeoTrellis Geometry have implicit conversions from the corresponding types in JTS.

If you do use JTS APIs directly, it is still straightforward to preserve immutability as most JTS operations also produce new geometries without modifying their inputs.
Known exceptions include:
+ `Geometry#normalize`
+ `Geometry#apply(CoordinateFilter)`
+ `Geometry#apply(CoordinateSequenceFilter)`
+ `Geometry#apply(GeometryComponentFilter)`
+ `Geometry#apply(GeometryFilter)`
+ `Geometry#setSRID(int)`
+ `Geometry#setUserData(AnyRef)`

So if you need immutability in your application, please take care in using the above methods.
For example, if you need to transform coordinates directly using a JTS CoordinateFilter, you would need to be careful to avoid modifying the original geometry:

```scala
    import geotrellis.vector._
    import com.vividsolutions.jts.{ geom => jts }
  
    /*
     * Translate by directly adding to the coordinates, rather than going
     * through a full matrix transform using the AffineTransform class.
     */
    def myTranslate(dx: Double, dy: Double, geom: Geometry): Geometry = {
      val jtsGeom = geom.unsafeGeom.clone().asInstanceOf[jts.Geometry]
      jtsGeom.apply(new jts.CoordinateFilter {
        def filter(coordinate: jts.Coordinate) {
          coordinate.x = coordinate.x + dx
          coordinate.y = coordinate.y + dy
        }
      })
      Geometry(jtsGeom)
    }
```

##Features
In GIS terminology, a "feature" is a discrete object in a geospatial dataset.
The GeoTrellis `Feature` represents features by storing a Geometry and associated data.

In Scala, Feature is declared:

```scala
case class Feature[+G <: Geometry, +D](geom: G, data: D)
```

Both the geometry type and the associated data are represented using type parameters, so Feature supports any combination of geometry and data while allowing retention of the specific type information.
Additionally, features are *covariant* in both the geometry and data type, so the subtyping relationships of features follow the subtyping relationships of those type parameters.

GeoTrellis adds *type aliases* for each concrete Geometry type that a Feature might have. For example, `Feature[Point, Weather]` can also be written as `PointFeature[Weather]`. Because this is a type alias the two are treated as exactly the same type by the compiler.

GeoTrellis provides JSON encoding and decoding facilities based on the spray-json library.
If instances of the JsonWriter and JsonReader typeclasses from spray-json are available for your datatype, you can serialize and deserialize features easily.
Spray-json provides simple tools for implementing JsonWriter and JsonReader for case classes:

```scala
import geotrellis.vector._
import geotrellis.vector.io.json._
import spray.json._
import spray.json.DefaultJsonProtocol._

case class Weather(temperature: Double, precipitation: Double, humidity: Double)

implicit val weatherFormat = jsonFormat3(Weather)
val report = Feature(Point(39.958,-75.160), Weather(50.7, 0, 61))
report.toGeoJson
```

This should produce the GeoJSON representation for the feature. In GeoJson, the data field becomes the `properties` member of the feature.
```
{"type":"Feature","geometry":{"type":"Point","coordinates":[39.958,-75.16]},"properties":{"temperature":50.7,"precipitation":0.0,"humidity":61.0}}
```

##GeoTrellis Extents

There's one more piece to the `geotrellis.vector` puzzle: `Extent`. a `geotrellis.vector.Extent` is nothing more than a rectangular polygon which is projected (look [here](../../../../../proj4/src/main/scalageotrellis/proj4) for more on projection). This is useful mainly as a tool for defining the extent covered by a tile - the two jointly yield a raster (for more on rasters, go [here](../../../../../raster/src/main/scalageotrellis/raster)).
Constructing these bad boys is pretty easy. Since they're rectangles, we only need to provide four unique points. Take a look at this source:
From Extent.scala:
```scala
case class Extent(xmin: Double, ymin: Double, xmax: Double, ymax: Double)
```
Not too shabby. But remember that the real sweet spot for these is their built in support for handling projections (seriously: go look at the link from above regarding projection if this is fuzzy).
From Extent.scala:
```scala
case class ProjectedExtent(extent: Extent, crs: CRS) {
  def reproject(dest: CRS): Extent = 
    extent.reproject(crs, dest)
}
```
Really, that's about all you need to know to get started with extents. They're a powerful tool for a tightly defined task.

##Submodules

These submodules define useful methods for dealing with the entities that call `geotrellis.vector` home:
- `geotrellis.vector.io` defines input/output (serialization) of geometries
- `geotrellis.vector.op` defines common operations on geometries
- `geotrellis.vector.reproject` defines methods for translating between projections
