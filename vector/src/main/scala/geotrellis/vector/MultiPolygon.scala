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
import com.vividsolutions.jts.geom.TopologyException

import com.vividsolutions.jts.{geom => jts}

import spire.syntax.cfor._

object MultiPolygon {
  lazy val EMPTY = MultiPolygon(Seq[Polygon]())

  def apply(ps: Polygon*): MultiPolygon =
    apply(ps)

  def apply(ps: Traversable[Polygon]): MultiPolygon =
    MultiPolygon(factory.createMultiPolygon(ps.map(_.unsafeGeom).toArray))

  def apply(ps: Array[Polygon]): MultiPolygon = {
    val len = ps.length
    val arr = Array.ofDim[jts.Polygon](len)
    cfor(0)(_ < len, _ + 1) { i =>
      arr(i) = ps(i).unsafeGeom
    }

    MultiPolygon(factory.createMultiPolygon(arr))
  }

  implicit def jts2MultiPolygon(unsafeGeom: jts.MultiPolygon): MultiPolygon = apply(unsafeGeom)
}

case class MultiPolygon(unsafeGeom: jts.MultiPolygon) extends MultiGeometry
                                                   with Relatable
                                                   with TwoDimensions {

  /** Returns a unique representation of the geometry based on standard coordinate ordering. */
  def normalized(): MultiPolygon = { 
    val geom = unsafeGeom.clone.asInstanceOf[jts.MultiPolygon]
    geom.normalize
    MultiPolygon(geom)
  }

  /** Returns the Polygons contained in MultiPolygon. */
  lazy val polygons: Array[Polygon] = {
    for (i <- 0 until unsafeGeom.getNumGeometries) yield {
      Polygon(unsafeGeom.getGeometryN(i).clone.asInstanceOf[jts.Polygon])
    }
  }.toArray

  lazy val area: Double =
    unsafeGeom.getArea

  lazy val boundary: MultiLineResult =
    unsafeGeom.getBoundary

  /** Returns this MulitPolygon's vertices. */
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

  def intersection(): MultiPolygonMultiPolygonIntersectionResult =
    polygons.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.intersection(_)
    }

  def &(p: Point): PointOrNoResult =
    intersection(p)
  def intersection(p: Point): PointOrNoResult =
    p.intersection(this)
  def safeIntersection(p: Point): PointOrNoResult =
    try intersection(p)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(p.unsafeGeom))
    }

  def &(l: Line): OneDimensionAtLeastOneDimensionIntersectionResult =
    intersection(l)
  def intersection(l: Line): OneDimensionAtLeastOneDimensionIntersectionResult =
    l.intersection(this)
  def safeIntersection(l: Line): OneDimensionAtLeastOneDimensionIntersectionResult =
    try intersection(l)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(l.unsafeGeom))
    }

  def &(g: TwoDimensions): TwoDimensionsTwoDimensionsIntersectionResult =
    intersection(g)
  def intersection(g: TwoDimensions): TwoDimensionsTwoDimensionsIntersectionResult =
    unsafeGeom.intersection(g.unsafeGeom)
  def safeIntersection(g: TwoDimensions): TwoDimensionsTwoDimensionsIntersectionResult =
    try intersection(g)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(g.unsafeGeom))
    }

  def &(ls: MultiLine): OneDimensionAtLeastOneDimensionIntersectionResult =
    intersection(ls)
  def intersection(ls: MultiLine): OneDimensionAtLeastOneDimensionIntersectionResult =
    ls.intersection(this)
  def safeIntersection(ls: MultiLine): OneDimensionAtLeastOneDimensionIntersectionResult =
    try intersection(ls)
    catch {
      case _: TopologyException => simplifier.reduce(unsafeGeom).intersection(simplifier.reduce(ls.unsafeGeom))
    }

  // -- Union

  def |(p: Point): PointMultiPolygonUnionResult =
    union(p)

  def union(p: Point): PointMultiPolygonUnionResult =
    unsafeGeom.union(p.unsafeGeom)

  def |(l: Line): LineMultiPolygonUnionResult =
    union(l)
  def union(l: Line): LineMultiPolygonUnionResult =
    l.union(this)

  def |(p: Polygon): TwoDimensionsTwoDimensionsUnionResult =
    union(p)

  def union(p: Polygon): TwoDimensionsTwoDimensionsUnionResult = {
    (this.polygons :+ p).toSeq.unionGeometries
  }

  def |(ps: MultiPoint): LineMultiPolygonUnionResult =
    union(ps)
  def union(ps: MultiPoint): LineMultiPolygonUnionResult =
    unsafeGeom.union(ps.unsafeGeom)

  def |(ls: MultiLine) = union(ls)
  def union(ls: MultiLine): LineMultiPolygonUnionResult =
    unsafeGeom.union(ls.unsafeGeom)

  def |(ps: MultiPolygon): TwoDimensionsTwoDimensionsUnionResult =
    union(ps)
  def union(ps: MultiPolygon): TwoDimensionsTwoDimensionsUnionResult =
    (this.polygons ++ ps.polygons).toSeq.unionGeometries

  def union: TwoDimensionsTwoDimensionsUnionResult =
    polygons.toSeq.unionGeometries

  // -- Difference

  def difference(): MultiPolygonMultiPolygonDifferenceResult =
    polygons.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.difference(_)
    }

  def -(p: Point): MultiPolygonXDifferenceResult =
    difference(p)
  def difference(p: Point): MultiPolygonXDifferenceResult =
    unsafeGeom.difference(p.unsafeGeom)

  def -(l: Line): MultiPolygonXDifferenceResult =
    difference(l)
  def difference(l: Line): MultiPolygonXDifferenceResult =
    unsafeGeom.difference(l.unsafeGeom)

  def -(p: Polygon): TwoDimensionsTwoDimensionsDifferenceResult =
    difference(p)
  def difference(p: Polygon): TwoDimensionsTwoDimensionsDifferenceResult =
    unsafeGeom.difference(p.unsafeGeom)

  def -(ps: MultiPoint): MultiPolygonXDifferenceResult =
    difference(ps)
  def difference(ps: MultiPoint): MultiPolygonXDifferenceResult =
    unsafeGeom.difference(ps.unsafeGeom)

  def -(ls: MultiLine): MultiPolygonXDifferenceResult =
    difference(ls)
  def difference(ls: MultiLine): MultiPolygonXDifferenceResult =
    unsafeGeom.difference(ls.unsafeGeom)

  def -(ps: MultiPolygon): TwoDimensionsTwoDimensionsDifferenceResult =
    difference(ps)
  def difference(ps: MultiPolygon): TwoDimensionsTwoDimensionsDifferenceResult =
    unsafeGeom.difference(ps.unsafeGeom)

  // -- SymDifference

  def symDifference(): MultiPolygonMultiPolygonSymDifferenceResult =
    polygons.map(_.unsafeGeom).reduce[jts.Geometry] {
      _.symDifference(_)
    }

  def symDifference(g: ZeroDimensions): PointMultiPolygonSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  def symDifference(g: OneDimension): LineMultiPolygonSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  def symDifference(g: TwoDimensions): TwoDimensionsTwoDimensionsSymDifferenceResult =
    unsafeGeom.symDifference(g.unsafeGeom)

  // -- Predicates

  def contains(g: Geometry): Boolean =
    unsafeGeom.contains(g.unsafeGeom)

  def coveredBy(g: TwoDimensions): Boolean =
    unsafeGeom.coveredBy(g.unsafeGeom)

  def covers(g: Geometry): Boolean =
    unsafeGeom.covers(g.unsafeGeom)

  def crosses(g: OneDimension): Boolean =
    unsafeGeom.crosses(g.unsafeGeom)

  def crosses(ps: MultiPoint): Boolean =
    unsafeGeom.crosses(ps.unsafeGeom)

  def overlaps(g: TwoDimensions): Boolean =
    unsafeGeom.crosses(g.unsafeGeom)

  def touches(g: AtLeastOneDimension): Boolean =
    unsafeGeom.touches(g.unsafeGeom)

  def within(g: TwoDimensions): Boolean =
    unsafeGeom.within(g.unsafeGeom)
}
