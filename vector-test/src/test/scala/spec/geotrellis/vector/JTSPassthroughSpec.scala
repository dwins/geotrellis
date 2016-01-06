package geotrellis.vector

import org.scalatest.FunSpec
import org.scalatest.Matchers

class JTSPassthroughSpec extends FunSpec with Matchers {
  val box = Polygon(Point(0,0), Point(0, 1), Point(1,1), Point(1,0), Point(0,0))
  val shiftedBox = Polygon(Point(0.5,0.5), Point(0.5, 1.5), Point(1.5,1.5), Point(1.5,0.5), Point(0.5,0.5))

  describe("Direct JTS Access") {
    // Case 1: Produces a geometry, leaving the existing geometry unchanged
    it("should support [Geometry => Geometry] operations") {
      box.centroid.toGeometry should be(Some(box.operate(VectorOp.compute(_.getCentroid))))
    }

    it("should support [Geometry => Unit] in-place mutations") {
      box.operate { VectorOp.mutate { jtsGeometry =>
        jtsGeometry.apply { new com.vividsolutions.jts.geom.CoordinateFilter {
          override def filter(c: com.vividsolutions.jts.geom.Coordinate): Unit = {
            c.x *= 2
            c.y *= 2
          }
        } }
        jtsGeometry
      } } should not(be(box))
    }

    it("should support [Geometry => A] operations (A not Geometry)") {
      box.area should be(box.evaluate(VectorOp.compute(_.getArea)))
    }

    it("should support [Geometry* => Geometry] operations") {
      val intersection = VectorOp.compute(a => VectorOp.compute(b => a.intersection(b)))
      val result = shiftedBox.operate(box.evaluate(intersection))
      (box & shiftedBox).toGeometry should be(Some(result))
    }

    it("should support [Geometry* => A] operations (A not Geometry)") {
      val distance = VectorOp.compute(a => VectorOp.compute(b => a.distance(b)))
      val result = shiftedBox.evaluate(box.evaluate(distance))
      (shiftedBox distance box) should be(result)
    }
  }
}
