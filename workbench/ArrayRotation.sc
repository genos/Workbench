object ArrayRotation {

  /**
   * Swaps the `i`th and `j`th entries of `xs` '''in place''', mutating the original array
   *
   * @param xs Array of `T` values
   * @param i  First index for swapping
   * @param j  Second index for swapping
   * @tparam T Underlying element type
   * @throws `ArrayIndexOutOfBoundsException` if `i` or `j` aren't legal indices in `xs`
   *
   * @example {{{
   * scala> val xs = (0 to 3).toArray
   * xs: Array[Int] = Array(0, 1, 2, 3)
   * scala> swapInPlace(xs, 0, 2)
   * scala> xs
   * res1: Array[Int] = Array(2, 1, 0, 3)
   * }}}
   */
  @inline def swapInPlace[@specialized(Boolean, Char, Double, Int) T](xs: Array[T], i: Int, j: Int): Unit = {
    val t: T = xs(i)
    xs(i) = xs(j)
    xs(j) = t
  }

  /**
   * Rotates an array of `T` '''in place''', mutating the original array.
   *
   * @param xs Array to rotate
   * @param k  Number of step to rotate
   * @tparam T Underlying element type
   *
   * @example {{{
   * scala> val xs = (0 to 3).toArray
   * xs: Array[Int] = Array(0, 1, 2, 3)
   * scala> rotateInPlace(xs, 2)
   * scala> xs
   * res1: Array[Int] = Array(2, 3, 0, 1)
   * }}}
   */
  def rotateInPlace[@specialized(Boolean, Char, Double, Int) T](xs: Array[T], k: Int): Unit = {
    val n: Int = xs.length
    if (!(k < 0 || k > n)) {
      var i: Int = 0
      var j: Int = n - 1
      var t: T = xs(0)
      while (i < j) {
        swapInPlace(xs, i, j)
        i += 1
        j -= 1
      }
      i = 0
      j = k - 1
      while (i < j) {
        swapInPlace(xs, i, j)
        i += 1
        j -= 1
      }
      i = k
      j = n - 1
      while (i < j) {
        swapInPlace(xs, i, j)
        i += 1
        j -= 1
      }
    }
  }

}

/** Executable unit tests for [[ArrayRotation]] */
object ArrayRotationUnitTests {

  import ArrayRotation._

  def main(args: Array[String]): Unit = {
    println("ArrayRotationUnitTests…")
    swapInPlaceTests
    rotateInPlaceTests
    println("…passed!")
  }

  /** Unit tests for [[ArrayRotation.swapInPlace]] */
  def swapInPlaceTests(): Unit = {
    // simple test
    val xs = (0 to 3).toArray
    swapInPlace(xs, 0, 2)
    sameElementsSameOrder(xs, Seq(2, 1, 0, 3))
    // swapping is its own inverse
    val ys = ('A' to 'Z').toArray
    val zs = ('A' to 'Z').toSeq
    (0 to 100) foreach { _ =>
      val i = scala.util.Random.nextInt(ys.length)
      val j = scala.util.Random.nextInt(ys.length)
      swapInPlace(ys, i, j)
      swapInPlace(ys, i, j)
      sameElementsSameOrder(ys, zs)
    }
    println("✓ swapInPlaceTests")
  }

  /** Unit tests for [[ArrayRotation.rotateInPlace]] */
  def rotateInPlaceTests(): Unit = {
    // simple test
    val xs = (0 to 3).toArray
    rotateInPlace(xs, 2)
    sameElementsSameOrder(xs, Seq(2, 3, 0, 1))
    // inversion test: rotate by i, then (length - i)
    val ys = ('A' to 'Z').toArray
    val zs = ('A' to 'Z').toSeq
    (0 to 100) foreach { _ =>
      val i = scala.util.Random.nextInt(ys.length)
      rotateInPlace(ys, i)
      rotateInPlace(ys, ys.length - i)
      sameElementsSameOrder(ys, zs)
    }
    println("✓ rotateInPlaceTests")
  }

  /**
   * Asserts that `xs` and `ys` have the same size and consist of the same
   * elements in the same order
   */
  def sameElementsSameOrder[T](xs: Iterable[T], ys: Iterable[T]): Unit = {
    assert(xs.size == ys.size, s"(#xs = ${xs.size}) ≠ (#ys = ${ys.size})")
    xs.zip(ys).zipWithIndex foreach {
      case ((x, y), i) =>
      assert(x == y, s"(xs[$i] = $x) ≠ (ys[$i] = $y)")
    }
  }

}
