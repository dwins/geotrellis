/*
 * Copyright (c) 2014 Azavea.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package geotrellis.vector

import com.vividsolutions.jts.{geom => jts}
import com.vividsolutions.jts.geom.TopologyException
import GeomFactory._
import geotrellis.proj4.CRS

/**
 * A planar, linear, vector geometry.  GeoTrellis uses the JTS library as the
 * basis for computational geometry algorithms, but provides its own Geometry
 * hierarchy to adapt the JTS API to one more convenient to Scala developers
 * and coherent with GeoTrellis design goals. Notably:
 *
 *   - The GeoTrellis Geometry hierarchy models the GeoJSON specification,
 *     instead of the slightly more complex Simple Features Specification used in
 *     JTS.
 *
 *   - Where JTS algorithms often return the bare 'Geometry' type, GeoTrellis
 *     tries to specify more concrete types when they are statically known.  For
 *     example, applying an affine transformation always produces the same type
 *     of Geometry that the transformation is being applied to, and this is
 *     reflected in GeoTrellis vector.
 *
 *   - In the case of algorithms that may produce a few different types of
 *     Geometry, but not all types, the GeometryResult type works with Scala's
 *     exhaustiveness checker to detect when calling code does not account for
 *     all possible results.  For example, the intersection between a Line and a
 *     Point can only be a Point, or the empty geometry, so this operation
 *     returns a PointOrNoResult.
 *
 *   - Extractors are provided to allow pattern matching over the contents
 *     of Geometries. For example, you can extract the coordinates of a point with:
 *     `val Point(x,y) = point`
 *
 * == JTS Interoperability ==
 * Every Geotrellis Geometry wraps a JTS Geometry.  When a Geotrellis Geometry
 * is constructed from a JTS Geometry it references that original object,
 * rather than making a defensive copy.  JTS Geometry objects do expose some
 * in-place mutating operations, so Geotrellis Geometry objects are also mutable.
 * However, Geotrellis ensures that appropriate copies are made when using
 * mutating operations, so it is usually safe to treat Geometry as immutable.
 *
 * In some cases JTS may provide functionality that has not been exposed in the
 * Geotrellis API.  In this case you can access the JTS Geometry underlying a
 * Geotrellis Geometry via the .unsafeGeom method. Subtypes of Geotrellis Geometry
 * have implicit conversions from the corresponding types in JTS.
 *
 * If you do use JTS APIs directly, it is still straightforward to preserve
 * immutability as most JTS operations also produce new geometries without
 * modifying their inputs. Known exceptions include:
 *   - `Geometry#normalize`
 *   - `Geometry#apply(CoordinateFilter)`
 *   - `Geometry#apply(CoordinateSequenceFilter)`
 *   - `Geometry#apply(GeometryComponentFilter)`
 *   - `Geometry#apply(GeometryFilter)`
 *   - `Geometry#setSRID()`
 *   - `Geometry#setUserData()`
 *
 * So if you need immutability in your application, please take care in using
 * the above methods. For example, if you need to transform the coordinates
 * directly using a JTS CoordinateFilter, you would need to be careful to avoid
 * modifying the original geometry:
 *
 * {{{
 * import geotrellis.vector._
 * import com.vividsolutions.jts.{ geom => jts }
 *
 * /*
 *  * Translate by directly adding to the coordinates, rather than going
 *  * through a full matrix transform using the AffineTransform class.
 *  */
 * def myTranslate(dx: Double, dy: Double, geom: Geometry): Geometry = {
 *   val jtsGeom = geom.unsafeGeom.clone().asInstanceOf[jts.Geometry]
 *   jtsGeom.apply(new jts.CoordinateFilter {
 *     def filter(coordinate: jts.Coordinate) {
 *       coordinate.x = coordinate.x + dx
 *       coordinate.y = coordinate.y + dy
 *     }
 *   })
 *   Geometry(jtsGeom)
 * }
 * }}}
 */
trait Geometry {

  def unsafeGeom: jts.Geometry

  def isValid: Boolean =
    unsafeGeom.isValid

  def distance(other: Geometry): Double =
    unsafeGeom.distance(other.unsafeGeom)

  def withinDistance(other: Geometry, dist: Double): Boolean =
    unsafeGeom.isWithinDistance(other.unsafeGeom, dist)

  def centroid: PointOrNoResult =
    unsafeGeom.getCentroid

  def interiorPoint: PointOrNoResult =
    unsafeGeom.getInteriorPoint

  def envelope: Extent =
    if(unsafeGeom.isEmpty) Extent(0.0, 0.0, 0.0, 0.0)
    else unsafeGeom.getEnvelopeInternal

  def &(g: Geometry): TwoDimensionsTwoDimensionsIntersectionResult =
    intersection(g)
  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g.
   */
  def intersection(g: Geometry): TwoDimensionsTwoDimensionsIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)
  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g. If it fails, it reduces the precision to avoid [[TopologyException]].
   */
  def safeIntersection(g: Geometry): TwoDimensionsTwoDimensionsIntersectionResult =
    try intersection(g)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(g.unsafeGeom))
    }

  override
  def equals(other: Any): Boolean =
    other match {
      case g: Geometry => unsafeGeom.equals(g.unsafeGeom)
      case _ => false
  }

  override
  def hashCode(): Int = unsafeGeom.hashCode

  override def toString = unsafeGeom.toString
}

object Geometry {
  /**
   * Wraps JTS Geometry in correct container and attempts to cast.
   * Useful when sourcing objects from JTS interface.
   */
  def apply[G <: Geometry](obj: jts.Geometry): G = {
    obj match {
      case obj: jts.Point => Point(obj)
      case obj: jts.LineString => Line(obj)
      case obj: jts.Polygon => Polygon(obj)
      case obj: jts.MultiPoint => MultiPoint(obj)
      case obj: jts.MultiLineString => MultiLine(obj)
      case obj: jts.MultiPolygon => MultiPolygon(obj)
      case obj: jts.GeometryCollection => GeometryCollection(obj)
    }
  }.asInstanceOf[G]
}

trait Relatable { self: Geometry =>

  def intersects(other: Geometry): Boolean =
    unsafeGeom.intersects(other.unsafeGeom)

  def disjoint(other: Geometry): Boolean =
    unsafeGeom.disjoint(other.unsafeGeom)

}
