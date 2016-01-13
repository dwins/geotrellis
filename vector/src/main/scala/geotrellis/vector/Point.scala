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

object Point {
  def apply(x: Double, y: Double): Point =
    Point(factory.createPoint(new jts.Coordinate(x, y)))

  def apply(t: (Double, Double)): Point =
    apply(t._1, t._2)

  implicit def jts2Point(unsafeGeom: jts.Point): Point = apply(unsafeGeom)

  implicit def jtsCoord2Point(coord: jts.Coordinate): Point = 
    Point(factory.createPoint(coord))
}

case class Point(unsafeGeom: jts.Point) extends Geometry
                                        with Relatable
                                        with ZeroDimensions {

  assert(!unsafeGeom.isEmpty)

  /** The Point's x-coordinate */
  val x: Double =
    unsafeGeom.getX

  /** The Point's y-coordinate */
  val y: Double =
    unsafeGeom.getY

  private[vector] def toCoordinate() =
    new jts.Coordinate(x, y)

  // -- Intersection

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def &(g: Point): PointOrNoResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def intersection(g: Point): PointOrNoResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def &(g: Line): PointOrNoResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def intersection(g: Line): PointOrNoResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def &(g: Polygon): PointOrNoResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def intersection(g: Polygon): PointOrNoResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def &(g: MultiPoint): PointOrNoResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def intersection(g: MultiPoint): PointOrNoResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def &(g: MultiLine): PointOrNoResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def intersection(g: MultiLine): PointOrNoResult =
    unsafeGeom.intersection(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def &(g: MultiPolygon): PointOrNoResult =
    intersection(g)

  /**
   * Computes a Result that represents a Geometry made up of the points shared
   * by this Point and g.
   */
  def intersection(g: MultiPolygon): PointOrNoResult =
    unsafeGeom.intersection(g.unsafeGeom)

  // -- Union

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in g.
   */
  def |(g: ZeroDimensions): PointZeroDimensionsUnionResult =
    union(g)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in g.
   */
  def union(g: ZeroDimensions): PointZeroDimensionsUnionResult =
    unsafeGeom.union(g.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in l.
   */
  def |(l: Line): ZeroDimensionsLineUnionResult =
    union(l)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in l.
   */
  def union(l: Line): ZeroDimensionsLineUnionResult =
    unsafeGeom.union(l.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in p.
   */
  def |(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    union(p)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in p.
   */
  def union(p: Polygon): AtMostOneDimensionPolygonUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in ml.
   */
  def |(ml: MultiLine): PointMultiLineUnionResult =
    union(ml)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in ml.
   */
  def union(ml: MultiLine): PointMultiLineUnionResult =
    unsafeGeom.union(ml.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in mp.
   */
  def |(mp: MultiPolygon): PointMultiPolygonUnionResult =
    union(mp)

  /**
   * Computes a Result that represents a Geometry made up of this Point and all
   * the points in mp.
   */
  def union(mp: MultiPolygon): PointMultiPolygonUnionResult =
    unsafeGeom.union(mp.unsafeGeom)

  // -- Difference

  /**
   * Computes a Result that represents a Geometry made up of this Point less
   * all the points in g.
   */
  def -(other: Geometry): PointGeometryDifferenceResult =
    difference(other)

  /**
   * Computes a Result that represents a Geometry made up of this Point less
   * all the points in g.
   */
  def difference(other: Geometry): PointGeometryDifferenceResult =
    unsafeGeom.difference(other.unsafeGeom)


  // -- SymDifference

  /**
   * Computes a Result that represents a Geometry made up of this Point, if it
   * is not in p, and p if it is not this Point.
   */
  def symDifference(p: Point): PointPointSymDifferenceResult =
    unsafeGeom.symDifference(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point, if it
   * is not in l, and all the points in l that are not this Point.
   */
  def symDifference(l: Line): ZeroDimensionsLineSymDifferenceResult =
    unsafeGeom.symDifference(l.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point, if it
   * is not in p, and all the points in p that are not this Point.
   */
  def symDifference(p: Polygon): AtMostOneDimensionPolygonSymDifferenceResult =
    unsafeGeom.symDifference(p.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point, if it
   * is not in mp, and all the points in mp that are not this Point.
   */
  def symDifference(mp: MultiPoint): ZeroDimensionsMultiPointSymDifferenceResult =
    unsafeGeom.symDifference(mp.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point, if it
   * is not in ml, and all the points in ml that are not this Point.
   */
  def symDifference(ml: MultiLine): PointMultiLineSymDifferenceResult =
    unsafeGeom.symDifference(ml.unsafeGeom)

  /**
   * Computes a Result that represents a Geometry made up of this Point, if it
   * is not in mp, and all the points in mp that are not this Point.
   */
  def symDifference(mp: MultiPolygon): PointMultiPolygonSymDifferenceResult =
    unsafeGeom.symDifference(mp.unsafeGeom)


  // -- Buffer

  /** Computes a buffer area around this Point having width d. */
  def buffer(d: Double): Polygon =
    unsafeGeom.buffer(d) match {
      case p: jts.Polygon => Polygon(p)
      case x =>
        sys.error(s"Unexpected result for Point buffer: ${x.getGeometryType}")
    }


  // -- Predicates

  /**
   * Tests whether this Point contains the specified ZeroDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF*.
   */
  def contains(g: ZeroDimensions): Boolean =
    unsafeGeom.contains(g.unsafeGeom)

  /**
   * Tests whether this Point is covered by the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is T*F**F*** or
   * *TF**F*** or **FT*F*** or **F*TF***.
   */
  def coveredBy(g: Geometry): Boolean =
    unsafeGeom.coveredBy(g.unsafeGeom)

  /**
   * Tests whether this Point covers the specified ZeroDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*****FF* or *T****FF* or ***T**FF* or ****T*FF*.
   */
  def covers(g: ZeroDimensions): Boolean =
    unsafeGeom.covers(g.unsafeGeom)

  /**
   * Tests whether this Point touches the specified AtLeastOneDimensions g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * FT*******, F**T***** or F***T****.
   */
  def touches(g: AtLeastOneDimension): Boolean =
    unsafeGeom.touches(g.unsafeGeom)

  /**
   * Tests whether this Point is within the specified Geometry g.
   * Returns true if the DE-9IM Intersection Matrix for the two geometries is
   * T*F**F***.
   */
  def within(g: Geometry): Boolean =
    unsafeGeom.within(g.unsafeGeom)
}
