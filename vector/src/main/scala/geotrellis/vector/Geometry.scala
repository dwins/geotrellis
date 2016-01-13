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
