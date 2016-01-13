package geotrellis.vector.affine

import geotrellis.vector._
import com.vividsolutions.jts.{geom => jts}
import com.vividsolutions.jts.geom.{util => jtsutil}

object AffineTransformation {
  def apply(): AffineTransformation =
    apply(new jtsutil.AffineTransformation)

  def apply(jtsTrans: jtsutil.AffineTransformation): AffineTransformation =
    new AffineTransformation { val trans = jtsTrans }
}

trait AffineTransformation {
  val trans: jtsutil.AffineTransformation

  def transform(geom: Point): Point = Point(transform(geom.unsafeGeom))
  def transform(geom: Line): Line = Line(transform(geom.unsafeGeom))
  def transform(geom: Polygon): Polygon = Polygon(transform(geom.unsafeGeom))
  def transform(geom: MultiPoint): MultiPoint = MultiPoint(transform(geom.unsafeGeom))
  def transform(geom: MultiLine): MultiLine = MultiLine(transform(geom.unsafeGeom))
  def transform(geom: MultiPolygon): MultiPolygon = MultiPolygon(transform(geom.unsafeGeom))
  def transform(geom: GeometryCollection): GeometryCollection = GeometryCollection(transform(geom.unsafeGeom))
  def transform(geom: Geometry): Geometry = Geometry(transform(geom.unsafeGeom))

  private def transform[D <: jts.Geometry](g: D): D = trans.transform(g).asInstanceOf[D]

  def reflect(x: Double, y: Double): AffineTransformation = AffineTransformation(trans.reflect(x, y))

  def reflect(x0: Double, y0: Double, x1: Double, y1: Double): AffineTransformation = AffineTransformation(trans.reflect(x0, y0, x1, y1))

  def rotate(theta: Double): AffineTransformation = AffineTransformation(trans.rotate(theta))
  def rotate(sinTheta: Double, cosTheta: Double): AffineTransformation = AffineTransformation(trans.rotate(sinTheta, cosTheta))

  def scale(xscale: Double, yscale: Double): AffineTransformation = AffineTransformation(trans.scale(xscale, yscale))

  def shear(xshear: Double, yshear: Double): AffineTransformation = AffineTransformation(trans.shear(xshear, yshear))

  def translate(x: Double, y: Double): AffineTransformation = AffineTransformation(trans.translate(x, y))

}

object Reflection {
  def apply(x: Double, y: Double): AffineTransformation = 
    AffineTransformation().reflect(x, y)

  def apply(x0: Double, y0: Double, x1: Double, y1: Double): AffineTransformation = 
    AffineTransformation().reflect(x0, y0, x1, y1)
}

object Rotation {
  def apply(theta: Double): AffineTransformation = 
    AffineTransformation().rotate(theta)

  def apply(sinTheta: Double, cosTheta: Double): AffineTransformation = 
    AffineTransformation().rotate(sinTheta, cosTheta)
}

object Scaling {
  def apply(xscale: Double, yscale: Double): AffineTransformation =
    AffineTransformation().scale(xscale, yscale)
}

object Shearing {
  def apply(xshear: Double, yshear: Double): AffineTransformation =
    AffineTransformation().shear(xshear, yshear)
}

object Translation {
  def apply(x: Double, y: Double): AffineTransformation =
    AffineTransformation().translate(x, y)
}
