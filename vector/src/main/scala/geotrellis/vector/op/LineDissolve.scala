package geotrellis.vector
package op

import com.vividsolutions.jts.{ geom => jts }

/**
 * Dissolve all lines in a (multi-)geometry to get a collection of linestrings
 * and points where no line segment is repeated.  Points, lines, and polygons are all handled.
 */
object LineDissolve extends VectorOp[jts.Geometry]{
  def owned(g: jts.Geometry): jts.Geometry =
    com.vividsolutions.jts.dissolve.LineDissolver.dissolve(g)

  def shared(g: jts.Geometry): jts.Geometry =
    com.vividsolutions.jts.dissolve.LineDissolver.dissolve(g)
}
