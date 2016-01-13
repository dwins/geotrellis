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

import com.vividsolutions.jts.geom.TopologyException
import com.vividsolutions.jts.{geom => jts}
import GeomFactory._
import geotrellis.vector._

import spire.syntax.cfor._

object Polygon {
  implicit def jtsToPolygon(unsafeGeom: jts.Polygon): Polygon =
    Polygon(unsafeGeom)

  def apply(exterior: Point*)(implicit d: DummyImplicit): Polygon =
    apply(Line(exterior), Set())

  def apply(exterior: Seq[Point]): Polygon =
    apply(Line(exterior), Set())

  def apply(exterior: Line): Polygon =
    apply(exterior, Set())

  def apply(exterior: Line, holes:Line*): Polygon = 
    apply(exterior, holes)

  def apply(exterior: Line, holes:Traversable[Line]): Polygon = {
    if(!exterior.isClosed) {
      sys.error(s"Cannot create a polygon with unclosed exterior: $exterior")
    }

    if(exterior.vertices.length < 4) {
      sys.error(s"Cannot create a polygon with exterior with less that 4 points: $exterior")
    }

    val extGeom = factory.createLinearRing(exterior.unsafeGeom.getCoordinates)

    val holeGeoms = (
      for (hole <- holes) yield {
        if (!hole.isClosed) {
          sys.error(s"Cannot create a polygon with an unclosed hole: $hole")
        } else {
          if (hole.vertices.length < 4)
            sys.error(s"Cannot create a polygon with a hole with less that 4 points: $hole")
          else
            factory.createLinearRing(hole.unsafeGeom.getCoordinates)
        }
      }).toArray

    val p = factory.createPolygon(extGeom, holeGeoms)
    // Sometimes polygons are invalid even if they aren't.
    // Try buffer(0) per http://tsusiatsoftware.net/jts/jts-faq/jts-faq.html#G
    if(!p.isValid) { p.buffer(0).asInstanceOf[jts.Polygon] }
    else { p }
  }
}

case class Polygon(unsafeGeom: jts.Polygon) extends Geometry 
                                            with Relatable
                                            with TwoDimensions {

  assert(!unsafeGeom.isEmpty, s"Polygon Empty: $unsafeGeom")

  /** Returns a unique representation of the geometry based on standard coordinate ordering. */
  def normalized(): Polygon = { 
    val geom = unsafeGeom.clone.asInstanceOf[jts.Polygon]
    geom.normalize
    Polygon(geom)
  }

  /** Tests whether this Polygon is a rectangle. */
  lazy val isRectangle: Boolean =
    unsafeGeom.isRectangle

  /** Returns the area of this Polygon. */
  lazy val area: Double =
    unsafeGeom.getArea

  /** Returns the exterior ring of this Polygon. */
  lazy val exterior: Line =
    Line(unsafeGeom.getExteriorRing.clone.asInstanceOf[jts.LineString])

  /** Returns the hole rings of this Polygon. */
  lazy val holes: Array[Line] = {
    for (i <- 0 until numberOfHoles) yield
      Line(unsafeGeom.getInteriorRingN(i).clone.asInstanceOf[jts.LineString])
  }.toArray

  /** Returns true if this Polygon contains holes */
  lazy val hasHoles: Boolean =
    numberOfHoles > 0

  /** Returns the number of holes in this Polygon */
  lazy val numberOfHoles: Int =
    unsafeGeom.getNumInteriorRing

  /**
   * Returns the boundary of this Polygon.
   * The boundary of a Polygon is the set of closed curves corresponding to its
   * exterior and interior boundaries.
   */
  lazy val boundary: PolygonBoundaryResult =
    unsafeGeom.getBoundary

  /** Returns this Polygon's vertices. */
  lazy val vertices: Array[Point] = {
    val coords = unsafeGeom.getCoordinates
    val arr = Array.ofDim[Point](coords.size)
    cfor(0)(_ < arr.size, _ + 1) { i =>
      val coord = coords(i)
      arr(i) = Point(coord.x, coord.y)
    }
    arr
  }

  /** Get the number of vertices in this geometry */
  lazy val vertexCount: Int = unsafeGeom.getNumPoints

  /**
   * Returns this Polygon's perimeter.
   * A Polygon's perimeter is the length of its exterior and interior
   * boundaries.
   */
  lazy val perimeter: Double =
    unsafeGeom.getLength

  // -- Intersection

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and p.
   */
  def &(p: Point): PointOrNoResult =
    intersection(p)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and p.
   */
  def intersection(p: Point): PointOrNoResult =
    unsafeGeom.intersection(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g. If it fails, it reduces the precision to avoid [[TopologyException]].
   */
  def safeIntersection(p: Point): PointOrNoResult =
    try intersection(p)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(p.unsafeGeom))
    }

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and mp.
   */
  def &(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    intersection(mp)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and mp.
   */
  def intersection(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g. If it fails, it reduces the precision to avoid [[TopologyException]].
   */
  def safeIntersection(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    try intersection(mp)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(mp.unsafeGeom))
    }

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g.
   */
  def &(g: OneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g.
   */
  def intersection(g: OneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g. If it fails, it reduces the precision to avoid [[TopologyException]].
   */
  def safeIntersection(g: OneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    try intersection(g)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(g.unsafeGeom))
    }

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g.
   */
  def &(g: TwoDimensions): TwoDimensionsTwoDimensionsIntersectionResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g.
   */
  def intersection(g: TwoDimensions): TwoDimensionsTwoDimensionsIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Polygon and g. If it fails, it reduces the precision to avoid [[TopologyException]].
   */
  def safeIntersection(g: TwoDimensions): TwoDimensionsTwoDimensionsIntersectionResult =
    try intersection(g)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(g.unsafeGeom))
    }

  // -- Union

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon and g.
   */
  def |(g: AtMostOneDimension): AtMostOneDimensionPolygonUnionResult =
    union(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon and g.
   */
  def union(g: AtMostOneDimension): AtMostOneDimensionPolygonUnionResult =
    unsafeGeom.union(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon and g.
   */
  def |(g: TwoDimensions): TwoDimensionsTwoDimensionsUnionResult =
    union(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon and g. Uses cascaded polygon union if g is a (multi)polygon
   * else falls back to default jts union method.
   */
  def union(g: TwoDimensions): TwoDimensionsTwoDimensionsUnionResult = g match {
    case p:Polygon => Seq(this, p).unionGeometries
    case mp:MultiPolygon => (this +: mp.polygons).toSeq.unionGeometries
    case _ => unsafeGeom.union(g.unsafeGeom)
  }



  // -- Difference

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon that are not in g.
   */
  def -(g: AtMostOneDimension): PolygonAtMostOneDimensionDifferenceResult =
    difference(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon that are not in g.
   */
  def difference(g: AtMostOneDimension): PolygonAtMostOneDimensionDifferenceResult =
    unsafeGeom.difference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon that are not in g.
   */
  def -(g: TwoDimensions): TwoDimensionsTwoDimensionsDifferenceResult =
    difference(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon that are not in g.
   */
  def difference(g: TwoDimensions): TwoDimensionsTwoDimensionsDifferenceResult =
    unsafeGeom.difference(g.unsafeGeom)


  // -- SymDifference

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon that are not in g and all the points in g that are not in
   * this Polygon.
   */
  def symDifference(g: AtMostOneDimension): AtMostOneDimensionPolygonSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Polygon that are not in g and all the points in g that are not in
   * this Polygon.
   */
  def symDifference(g: TwoDimensions): TwoDimensionsTwoDimensionsSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)


  // -- Buffer


  /** Computes a buffer area around this Polygon having width d. */
  def buffer(d: Double): Polygon =
    unsafeGeom.buffer(d) match {
      case p: jts.Polygon => Polygon(p)
      case x =>
        sys.error(s"Unexpected result for Polygon buffer: ${x.getGeometryType}")
    }

  // -- Predicates


  /**
   * Tests whether this Polygon contains the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF*.
   */
  def contains(g: Geometry): Boolean =
    unsafeGeom.contains(g.unsafeGeom)

  /**
   * Tests whether this Polygon is covered by the specified TwoDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is T*F**F*** or
   * *TF**F*** or **FT*F*** or **F*TF***.
   */
  def coveredBy(g: TwoDimensions): Boolean =
    unsafeGeom.coveredBy(g.unsafeGeom)

  /**
   * Tests whether this Polygon covers the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF* or *T****FF* or ***T**FF* or ****T*FF*.
   */
  def covers(g: Geometry): Boolean =
    unsafeGeom.covers(g.unsafeGeom)

  /**
   * Tests whether this Polygon crosses the specified MultiPoint mp.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****T** (A/P).
   */
  def crosses(mp: MultiPoint): Boolean =
    unsafeGeom.crosses(mp.unsafeGeom)

  /**
   * Tests whether this Polygon crosses the specified OneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****T** (A/L).
   */
  def crosses(g: OneDimension): Boolean =
    unsafeGeom.crosses(g.unsafeGeom)

  /**
   * Tests whether this Polygon overlaps the specified TwoDimensions g.
   * Returns true if The DE-9IM Intersection Matrix for the two geometries is
   * T*T***T**.
   */
  def overlaps(g: TwoDimensions): Boolean =
    unsafeGeom.overlaps(g.unsafeGeom)

  /**
   * Tests whether this Polygon touches the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * FT*******, F**T***** or F***T****.
   */
  def touches(g: Geometry): Boolean =
    unsafeGeom.touches(g.unsafeGeom)

  /**
   * Tests whether this Polygon is within the specified TwoDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*F**F***.
   */
  def within(g: TwoDimensions): Boolean =
    unsafeGeom.within(g.unsafeGeom)
}
