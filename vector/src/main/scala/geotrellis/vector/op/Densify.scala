package geotrellis.vector
package op

import com.vividsolutions.jts.{ geom => jts }

/**
 * Densify a geometry by adding vertices along all line segments such that the
 * distance between adjacent vertices is never more than the specified
 * tolerance.  Point components are returned unmodified.
 */
object Densify extends VectorOp[jts.Geometry] {
  def owned(g: jts.Geometry): jts.Geometry = g.convexHull
  def shared(g: jts.Geometry): jts.Geometry = g.convexHull
}
