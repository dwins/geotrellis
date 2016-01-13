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

import com.vividsolutions.jts.{geom=>jts}

import scala.collection.JavaConversions._

import spire.syntax.cfor._

object MultiLine {
  lazy val EMPTY = MultiLine(Seq[Line]())

  def apply(ls: Line*): MultiLine = 
    MultiLine(ls)

  def apply(ls: Traversable[Line]): MultiLine = 
    MultiLine(factory.createMultiLineString(ls.map(_.unsafeGeom).toArray))

  def apply(ls: Array[Line]): MultiLine = {
    val len = ls.length
    val arr = Array.ofDim[jts.LineString](len)
    cfor(0)(_ < len, _ + 1) { i =>
      arr(i) = ls(i).unsafeGeom
    }

    MultiLine(factory.createMultiLineString(arr))
  }

  implicit def jts2MultiLine(unsafeGeom: jts.MultiLineString): MultiLine = apply(unsafeGeom)
}

case class MultiLine(unsafeGeom: jts.MultiLineString) extends MultiGeometry 
                                                      with Relatable
                                                      with OneDimension {

  /** Returns a unique representation of the geometry based on standard coordinate ordering. */
  def normalized(): MultiLine = { 
    val geom = unsafeGeom.clone.asInstanceOf[jts.MultiLineString]
    geom.normalize
    MultiLine(geom)
  }

  /** Returns the Lines contained in this MultiLine. */
  lazy val lines: Array[Line] = {
    for (i <- 0 until unsafeGeom.getNumGeometries) yield {
      Line(unsafeGeom.getGeometryN(i).clone.asInstanceOf[jts.LineString])
    }
  }.toArray

  /** Tests if the initial vertex equals the final vertex for every Line in
    * this MultiLine. */
  lazy val isClosed: Boolean =
    unsafeGeom.isClosed

  /**
   * Returns the boundary of this MultiLine.
   * The boundary of a non-closed MultiLine consists of all the end points of
   * the non-closed lines that make up the MultiLine. The boundary of a closed
   * MultiLine is empty.
   */
  lazy val boundary: OneDimensionBoundaryResult =
    unsafeGeom.getBoundary

  /** Returns this MulitLine's vertices. */
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
  def intersection(): MultiLineMultiLineIntersectionResult =
    lines.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.intersection(_)
    }

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiLine and p.
   */
  def &(p: Point): PointOrNoResult =
    intersection(p)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiLine and p.
   */
  def intersection(p: Point): PointOrNoResult =
    unsafeGeom.intersection(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiLine and g.
   */
  def &(g: AtLeastOneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiLine and g.
   */
  def intersection(g: AtLeastOneDimension): OneDimensionAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiLine and mp.
   */
  def &(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    intersection(mp)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this MultiLine and mp.
   */
  def intersection(mp: MultiPoint): MultiPointAtLeastOneDimensionIntersectionResult =
    unsafeGeom.intersection(mp.unsafeGeom)


  // -- Union

  /**
    * Computes the union of contained lines.
    * Useful for merging overlapping line segments.
    */
  def union(): MultiLineMultiLineUnionResult =
    unsafeGeom.union

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and p.
   */
  def |(p: Point): PointMultiLineUnionResult =
    union(p)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and p.
   */
  def union(p: Point): PointMultiLineUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and l.
   */
  def |(l: Line): LineOneDimensionUnionResult =
    union(l)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and l.
   */
  def union(l:Line): LineOneDimensionUnionResult =
    unsafeGeom.union(l.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and p.
   */
  def |(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    union(p)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and p.
   */
  def union(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and mp.
   */
  def |(mp: MultiPoint): MultiPointMultiLineUnionResult =
    union(mp)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and mp.
   */
  def union(mp: MultiPoint): MultiPointMultiLineUnionResult =
    unsafeGeom.union(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and ml.
   */
  def |(ml: MultiLine): MultiLineMultiLineUnionResult =
    union(ml)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and ml.
   */
  def union(ml: MultiLine): MultiLineMultiLineUnionResult =
    unsafeGeom.union(ml.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and mp.
   */
  def |(mp: MultiPolygon): MultiLineMultiPolygonUnionResult =
    union(mp)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine and mp.
   */
  def union(mp: MultiPolygon): MultiLineMultiPolygonUnionResult =
    unsafeGeom.union(mp.unsafeGeom)


  // -- Difference

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * the first line that are not in the other contained lines.
   */
  def difference(): MultiLineMultiLineDifferenceResult =
    lines.map(_.unsafeGeom).reduce[jts.Geometry] { 
      _.difference(_)
    }

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in g.
   */
  def -(g: Geometry): MultiLineGeometryDifferenceResult =
    difference(g)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in g.
   */
  def difference(g: Geometry): MultiLineGeometryDifferenceResult =
    unsafeGeom.difference(g.unsafeGeom)


  // -- SymDifference


  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * the contained lines that are unique to one line.
   */
  def symDifference(): MultiLineMultiLineSymDifferenceResult =
    lines.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.symDifference(_)
    }

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in p and the point p if it is not in this
   * MultiLine.
   */
  def symDifference(p: Point): PointMultiLineSymDifferenceResult =
    unsafeGeom.symDifference(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in mp and all the points in mp that are not in
   * this MultiLine.
   */
  def symDifference(mp: MultiPoint): MultiPointMultiLineSymDifferenceResult =
    unsafeGeom.symDifference(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in g and all the points in g that are not in
   * this MultiLine.
   */
  def symDifference(g: OneDimension): OneDimensionOneDimensionSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in p and all the points in p that are not in
   * this MultiLine.
   */
  def symDifference(p: Polygon): AtMostOneDimensionPolygonSymDifferenceResult =
    unsafeGeom.symDifference(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of all the points in
   * this MultiLine that are not in mp and all the points in mp that are not in
   * this MultiLine.
   */
  def symDifference(mp: MultiPolygon): MultiLineMultiPolygonSymDifferenceResult =
    unsafeGeom.symDifference(mp.unsafeGeom)


  // -- Predicates


  /**
   * Tests whether this MultiLine contains the specified AtMostOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF*.
   */
  def contains(g: AtMostOneDimension): Boolean =
    unsafeGeom.contains(g.unsafeGeom)

  /**
   * Tests whether this MultiLine is covered by the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is T*F**F*** or
   * *TF**F*** or **FT*F*** or **F*TF***.
   */
  def coveredBy(g: AtLeastOneDimension): Boolean =
    unsafeGeom.coveredBy(g.unsafeGeom)

  /**
   * Tests whether this MultiLine covers the specified AtMostOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF* or *T****FF* or ***T**FF* or ****T*FF*.
   */
  def covers(g: AtMostOneDimension): Boolean =
    unsafeGeom.covers(g.unsafeGeom)

  /**
   * Tests whether this MultiLine crosses the specified MultiPoint mp.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****T** (L/P).
   */
  def crosses(mp: MultiPoint): Boolean =
    unsafeGeom.crosses(mp.unsafeGeom)

  /**
   * Tests whether this MultiLine crosses the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * 0******** (L/L) or T*****T** (L/P and L/A).
   */
  def crosses(g: AtLeastOneDimension): Boolean =
    unsafeGeom.crosses(g.unsafeGeom)

  /**
   * Tests whether this MultiLine overlaps the specified OneDimension g.
   * Returns true if The DE-9IM Intersection Matrix for the two geometries is
   * 1*T***T**.
   */
  def overlaps(g: OneDimension): Boolean =
    unsafeGeom.overlaps(g.unsafeGeom)

  /**
   * Tests whether this MultiLine touches the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * FT*******, F**T***** or F***T****.
   */
  def touches(g: Geometry): Boolean =
    unsafeGeom.touches(g.unsafeGeom)

  /**
   * Tests whether this MultiLine is within the specified AtLeastOneDimension g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*F**F***.
   */
  def within(g: AtLeastOneDimension): Boolean =
    unsafeGeom.within(g.unsafeGeom)
}
