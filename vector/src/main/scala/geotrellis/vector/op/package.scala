package geotrellis.vector

package object op {
  implicit class LineDissolveWrapper(val lines: Traversable[Line]) {
    def dissolve(): Geometry = lines.ml.operate(LineDissolve)
  }
}
