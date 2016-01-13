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
import GeomFactory._

import spire.syntax.cfor._

object Line {

  implicit def jtsToLine(unsafeGeom: jts.LineString): Line =
    apply(unsafeGeom)

  def apply(points: (Double, Double)*)(implicit d: DummyImplicit): Line =
    apply(points)

  def apply(points: Traversable[(Double, Double)])(implicit d: DummyImplicit): Line =
    apply(points.map { case (x,y) => Point(x,y) })

  def apply(points: Point*): Line =
    apply(points.toList)

  def apply(points: Traversable[Point]): Line = {
    if (points.size < 2) {
      sys.error("Invalid line: Requires 2 or more points.")
    }

    val pointArray = points.toArray
    val coords = Array.ofDim[jts.Coordinate](pointArray.size)
    cfor(0)(_ < pointArray.size, _ + 1) { i =>
      coords(i) = pointArray(i).unsafeGeom.getCoordinate
    }

    Line(factory.createLineString(coords))
  }
}

case class Line(unsafeGeom: jts.LineString) extends Geometry
                                            with Relatable
                                            with OneDimension {

  assert(!unsafeGeom.isEmpty, s"LineString Empty: $unsafeGeom")

  /** Returns a unique representation of the geometry based on standard coordinate ordering. */
  def normalized(): Line = { 
    val geom = unsafeGeom.clone.asInstanceOf[jts.LineString]
    geom.normalize
    Line(geom)
  }

  /** Tests if the initial vertex equals the final vertex. */
  lazy val isClosed: Boolean =
    unsafeGeom.isClosed

  /**
   * Tests whether this Line is simple.
   * A Line is simple iff it does not self-intersect at points other than
   * boundary points.
   */
  lazy val isSimple: Boolean =
    unsafeGeom.isSimple

  /**
   * Returns the boundary of this Line.
   * The boundary of a non-closed Line consists of its two end points. The
   * boundary of a closed Line is empty.
   */
  lazy val boundary: OneDimensionBoundaryResult =
    unsafeGeom.getBoundary

  def points: Array[Point] = vertices

  /** Returns this Line's vertices. */
  def vertices: Array[Point] = {
    val size = unsafeGeom.getNumPoints
    val arr = Array.ofDim[Point](size)
    cfor(0)(_ < arr.size, _ + 1) { i =>
      val p = unsafeGeom.getPointN(i).clone.asInstanceOf[jts.Point]
      arr(i) = Point(p)
    }
    arr
  }

  /** Get the number of vertices in this geometry */
  lazy val vertexCount: Int = unsafeGeom.getNumPoints

  /** Returns the length of this Line. */
  lazy val length: Double =
    unsafeGeom.getLength

  /** Returns a closed version of the line. If already closed, just return this. */
  def closed(): Line =
    if(isClosed) this
    else {
      Line(vertices :+ vertices.head)
    }

  // -- Intersection

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Line and p.
   */
  def &(p: Point): PointOrNoResult =
    intersection(p)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Line and p.
   */
  def intersection(p: Point): PointOrNoResult =
    unsafeGeom.intersection(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Line and mp.
   */
  def &(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    intersection(mp)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Line and mp.
   */
  def intersection(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Line and g.
   */
  def &(g: AtLeastOneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Line and g.
   */

  def intersection(g: AtLeastOneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)

  // -- Union

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and g.
   */
  def |(g: ZeroDimensions): ZeroDimensionsLineUnionResult =
    union(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and g.
   */
  def union(g: ZeroDimensions): ZeroDimensionsLineUnionResult =
    unsafeGeom.union(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and g.
   */
  def |(g: OneDimension): LineOneDimensionUnionResult =
    union(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and g.
   */
  def union(g: OneDimension): LineOneDimensionUnionResult =
    unsafeGeom.union(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and p.
   */
  def |(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    union(p)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and p.
   */
  def union(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and mp.
   */
  def |(mp: MultiPolygon): LineMultiPolygonUnionResult =
    union(mp)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line and mp.
   */
  def union(mp: MultiPolygon): LineMultiPolygonUnionResult =
    unsafeGeom.union(mp.unsafeGeom)

  // -- Difference

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in g.
   */
  def -(g: ZeroDimensions): LineResult =
    difference(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in g.
   */
  def difference(g: ZeroDimensions): LineResult =
    unsafeGeom.difference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in g.
   */
  def -(g: AtLeastOneDimension): LineAtLeastOneDimensionDifferenceResult =
    difference(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in g.
   */
  def difference(g: AtLeastOneDimension): LineAtLeastOneDimensionDifferenceResult =
    unsafeGeom.difference(g.unsafeGeom)


  // -- SymDifference

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in g and all the points in g that are not in
   * this Line.
   */
  def symDifference(g: ZeroDimensions): ZeroDimensionsLineSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in g and all the points in g that are not in
   * this Line.
   */
  def symDifference(g: OneDimension): OneDimensionOneDimensionSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in p and all the points in p that are not in
   * this Line.
   */
  def symDifference(p: Polygon): AtMostOneDimensionPolygonSymDifferenceResult =
    unsafeGeom.symDifference(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this Line that are not in mp and all the points in mp that are not in
   * this Line.
   */
  def symDifference(mp: MultiPolygon): LineMultiPolygonSymDifferenceResult =
    unsafeGeom.symDifference(mp.unsafeGeom)


  // -- Buffer


  /** Computes a buffer area around this Line having width d. */
  def buffer(d: Double): Polygon =
    unsafeGeom.buffer(d) match {
      case p: jts.Polygon => Polygon(p)
      case x =>
        sys.error(s"Unexpected result for Line buffer: ${x.getGeometryType}")
    }


  // -- Predicates

  /**
   * Tests whether this Line contains the specified AtMostOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF*.
   */
  def contains(g: AtMostOneDimension): Boolean =
    unsafeGeom.contains(g.unsafeGeom)

  /**
   * Tests whether this Line is covered by the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is T*F**F*** or
   * *TF**F*** or **FT*F*** or **F*TF***.
   */
  def coveredBy(g: AtLeastOneDimension): Boolean =
    unsafeGeom.coveredBy(g.unsafeGeom)

  /**
   * Tests whether this Line covers the specified AtMostOneDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF* or *T****FF* or ***T**FF* or ****T*FF*.
   */
  def covers(g: AtMostOneDimension): Boolean =
    unsafeGeom.covers(g.unsafeGeom)

  /**
   * Tests whether this Line crosses the specified MultiPoint mp.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****T** (L/P).
   */
  def crosses(mp: MultiPoint): Boolean =
    unsafeGeom.crosses(mp.unsafeGeom)

  /**
   * Tests whether this Line crosses the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * 0******** (L/L) or T*T****** (L/A).
   */
  def crosses(g: AtLeastOneDimension): Boolean =
    unsafeGeom.crosses(g.unsafeGeom)

  /**
   * Tests whether this Line overlaps the specified OneDimension g.
   * Returns true if The DE-9IM Intersection Matrix for the two geometries is
   * 1*T***T**.
   */
  def overlaps(g: OneDimension): Boolean =
    unsafeGeom.overlaps(g.unsafeGeom)

  /**
   * Tests whether this Line touches the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * FT*******, F**T***** or F***T****.
   */
  def touches(g: Geometry): Boolean =
    unsafeGeom.touches(g.unsafeGeom)

  /**
   * Tests whether this Line is within the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*F**F***.
   */
  def within(g: AtLeastOneDimension): Boolean =
    unsafeGeom.within(g.unsafeGeom)
}
