package geotrellis.vector
package op

import com.vividsolutions.jts.{ geom => jts }

/**
 * Compute the convex hull for a geometry.  The convex hull is the smallest
 * convex shape that contains all points in the original geometry.
 */
object ConvexHull extends VectorOp[jts.Geometry] {
  def owned(g: jts.Geometry): jts.Geometry = g.convexHull
  def shared(g: jts.Geometry): jts.Geometry = g.convexHull
}
