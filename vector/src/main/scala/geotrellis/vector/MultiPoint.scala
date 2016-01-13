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

import GeomFactory._

import com.vividsolutions.jts.{geom => jts}

import spire.syntax.cfor._

object MultiPoint {
  lazy val EMPTY = MultiPoint(Seq[Point]())

  def apply(ps: Point*): MultiPoint = 
    apply(ps)

  def apply(ps: Traversable[Point]): MultiPoint =
    MultiPoint(factory.createMultiPoint(ps.map(_.unsafeGeom).toArray))

  def apply(ps: Array[Point]): MultiPoint = {
    val len = ps.length
    val arr = Array.ofDim[jts.Point](len)
    cfor(0)(_ < len, _ + 1) { i =>
      arr(i) = ps(i).unsafeGeom
    }

    MultiPoint(factory.createMultiPoint(arr))
  }

  def apply(ps: Traversable[(Double, Double)])(implicit d: DummyImplicit): MultiPoint =
    MultiPoint(factory.createMultiPoint(ps.map { p => new jts.Coordinate(p._1, p._2) }.toArray))

  implicit def jts2MultiPoint(unsafeGeom: jts.MultiPoint): MultiPoint = apply(unsafeGeom)
}

case class MultiPoint(unsafeGeom: jts.MultiPoint) extends MultiGeometry 
                                                  with Relatable
                                                  with ZeroDimensions {

  /** Returns a unique representation of the geometry based on standard coordinate ordering. */
  def normalized(): MultiPoint = { 
    val geom = unsafeGeom.clone.asInstanceOf[jts.MultiPoint]
    geom.normalize
    MultiPoint(geom)
  }

  /** Returns the Points contained in MultiPoint. */
  lazy val points: Array[Point] = vertices

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

  // -- Intersection

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by the contained lines.
   */
  def &(): MultiPointMultiPointIntersectionResult =
    intersection()

  def intersection(): MultiPointMultiPointIntersectionResult =
    points.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.intersection(_)
    }

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiPoint and p.
   */
  def &(p: Point): PointOrNoResult =
    intersection(p)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiPoint and p.
   */
  def intersection(p: Point): PointOrNoResult =
    unsafeGeom.intersection(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiPoint and mp.
   */
  def &(mp: MultiPoint): MultiPointMultiPointIntersectionResult =
    intersection(mp)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiPoint and mp.
   */
  def intersection(mp: MultiPoint): MultiPointMultiPointIntersectionResult =
    unsafeGeom.intersection(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiPoint and g.
   */
  def &(g: AtLeastOneDimension): MultiPointAtLeastOneDimensionIntersectionResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiPoint and g.
   */
  def intersection(g: AtLeastOneDimension): MultiPointAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)


  // -- Union

  /**
    * Computes the union of the contained points.
    * Useful for de-duplication.
    */
  def union(): MultiPointMultiPointUnionResult =
    unsafeGeom.union

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and p.
   */
  def |(p: Point): PointZeroDimensionsUnionResult =
    union(p)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and p.
   */
  def union(p: Point): PointZeroDimensionsUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and l.
   */
  def |(l: Line): ZeroDimensionsLineUnionResult =
    union(l)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and l.
   */
  def union(l: Line): ZeroDimensionsLineUnionResult =
    unsafeGeom.union(l.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and p.
   */
  def |(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    union(p)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and p.
   */
  def union(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and mp.
   */
  def |(mp: MultiPoint): MultiPointMultiPointUnionResult =
    union(mp)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and mp.
   */
  def union(mp: MultiPoint): MultiPointMultiPointUnionResult =
    unsafeGeom.union(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and ml.
   */
  def |(ml: MultiLine): MultiPointMultiLineUnionResult =
    union(ml)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and ml.
   */
  def union(ml: MultiLine): MultiPointMultiLineUnionResult =
    unsafeGeom.union(ml.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and mp.
   */
  def |(mp: MultiPolygon): MultiPointMultiPolygonUnionResult =
    union(mp)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint and mp.
   */
  def union(mp: MultiPolygon): MultiPointMultiPolygonUnionResult =
    unsafeGeom.union(mp.unsafeGeom)


  // -- Difference

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * the first line not in the other contained lines.
   */
  def difference(): MultiPointMultiPointDifferenceResult =
    points.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.difference(_)
    }

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in g.
   */
  def -(g: Geometry): MultiPointGeometryDifferenceResult =
    difference(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in g.
   */
  def difference(g: Geometry): MultiPointGeometryDifferenceResult =
    unsafeGeom.difference(g.unsafeGeom)


  // -- SymDifference

  /**
   * Computes a Result that represents a Geometry made up of all the unique
   * points in this MultiPoint.
   */
  def symDifference(): MultiPointMultiPointSymDifferenceResult =
    points.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.symDifference(_)
    }

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in g and all the points in g that are not in
   * this MultiPoint.
   */
  def symDifference(g: ZeroDimensions): ZeroDimensionsMultiPointSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in l and all the points in l that are not in
   * this MultiPoint.
   */
  def symDifference(l: Line): ZeroDimensionsLineSymDifferenceResult =
    unsafeGeom.symDifference(l.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in p and all the points in p that are not in
   * this MultiPoint.
   */
  def symDifference(p: Polygon): AtMostOneDimensionPolygonSymDifferenceResult =
    unsafeGeom.symDifference(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in ml and all the points in ml that are not in
   * this MultiPoint.
   */
  def symDifference(ml: MultiLine): MultiPointMultiLineSymDifferenceResult =
    unsafeGeom.symDifference(ml.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiPoint that are not in mp and all the points in mp that are not in
   * this MultiPoint.
   */
  def symDifference(mp: MultiPolygon): MultiPointMultiPolygonSymDifferenceResult =
    unsafeGeom.symDifference(mp.unsafeGeom)


  // -- Misc.


  /**
   * Computes the smallest convex Polygon that contains all the points in the
   * MultiPoint. Applies only to MultiPoints with three or more points.
   *
   * TODO: Assert that the MultiPoint has at least 3 points. Investigate the
   * case where given 3 points that form a straight line, convexHull() returns
   * a line instead of a polygon.
   */
  def convexHull(): Polygon =
    unsafeGeom.convexHull() match {
      case p: jts.Polygon => Polygon(p)
      case x =>
        sys.error(s"Unexpected result for MultiPoint convexHull: ${x.getGeometryType}")
    }


  // -- Predicates


  /**
   * Tests whether this MultiPoint contains the specified ZeroDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF*.
   */
  def contains(g: ZeroDimensions): Boolean =
    unsafeGeom.contains(g.unsafeGeom)

  /**
   * Tests whether this MultiPoint is covered by the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*F**F*** or *TF**F*** or **FT*F*** or **F*TF***.
   */
  def coveredBy(g: Geometry): Boolean =
    unsafeGeom.coveredBy(g.unsafeGeom)

  /**
   * Tests whether this MultiPoint covers the specified ZeroMostOneDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF* or *T****FF* or ***T**FF* or ****T*FF*.
   */
  def covers(g: ZeroDimensions): Boolean =
    unsafeGeom.covers(g.unsafeGeom)

  /**
    * Tests whether this MultiPoint crosses the specified AtLeastOneDimension g.
    * Returns true if the DE-9IM Intersection Matrix for the two geometries is
    * T*T****** (P/L and P/A).
    */
  def crosses(g: AtLeastOneDimension): Boolean =
    unsafeGeom.crosses(g.unsafeGeom)

  /**
   * Tests whether this MultiPoint overlaps the specified MultiPoint mp.
   * Returns true if The DE-9IM Intersection Matrix for the two MultiPoints is
   * T*T***T**.
   */
  def overlaps(mp: MultiPoint): Boolean =
    unsafeGeom.overlaps(mp.unsafeGeom)

  /**
   * Tests whether this MultiPoint touches the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * FT*******, F**T***** or F***T****.
   */
  def touches(g: AtLeastOneDimension): Boolean =
    unsafeGeom.touches(g.unsafeGeom)

  /**
   * Tests whether this MultiPoint is within the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*F**F***.
   */
  def within(g: Geometry): Boolean =
    unsafeGeom.within(g.unsafeGeom)
}
