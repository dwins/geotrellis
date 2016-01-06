package geotrellis.vector
package op

import com.vividsolutions.jts.{ geom => jts }

/**
 * Simplify geometries to reduce the number of vertices while minimally
 * changing the shape of the geometry.  This is done using the
 * Visvalingam-Whyatt algorithm.
 */
object Simplify {
  def apply(tolerance: Double): VectorOp[jts.Geometry] =
    VectorOp.compute(com.vividsolutions.jts.simplify.VWSimplifier.simplify(_, tolerance))
}
