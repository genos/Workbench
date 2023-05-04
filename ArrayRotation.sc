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
  @inline
  def swapInPlace[@specialized(Boolean, Char, Double, Float) T](
    xs: Array[T],
    i: Int,
    j: Int
  ): Unit = {
    val t: T = xs(i)
    xs(i) = xs(j)
    xs(j) = t
  }

  /**
    * Swaps the `i`th and `j`th entries of `xs` '''in place''', mutating the original array
    *
    * @param xs Array of boolean values
    * @param i  First index for swapping
    * @param j  Second index for swapping
    * @throws `ArrayIndexOutOfBoundsException` if `i` or `j` aren't legal indices in `xs`
    *
    * @example {{{
    * scala> val bs = Array(true, false, true)
    * xs: Array[Boolean] = Array(true, false, true)
    * scala> swapInPlace(bs, 0, 1)
    * scala> bs
    * res1: Array[Boolean] = Array(false, true, true)
    * }}}
    */
  @inline def swapInPlace(bs: Array[Boolean], i: Int, j: Int): Unit = {
    bs(i) ^= bs(j)
    bs(j) ^= bs(i)
    bs(i) ^= bs(j)
  }

  /**
    * Swaps the `i`th and `j`th entries of `xs` '''in place''', mutating the original array
    *
    * @param xs Array of integer values
    * @param i  First index for swapping
    * @param j  Second index for swapping
    * @tparam T Underlying element type
    * @throws `ArrayIndexOutOfBoundsException` if `i` or `j` aren't legal indices in `xs`
    *
    * @example {{{
    * scala> val is = (0 to 3).toArray
    * xs: Array[Int] = Array(0, 1, 2, 3)
    * scala> swapInPlace(is, 0, 2)
    * scala> is
    * res1: Array[Int] = Array(2, 1, 0, 3)
    * }}}
    */
  @inline def swapInPlace(is: Array[Int], i: Int, j: Int): Unit = {
    is(i) ^= is(j)
    is(j) ^= is(i)
    is(i) ^= is(j)
  }

  /**
    * Swaps the `i`th and `j`th entries of `xs` '''in place''', mutating the original array
    *
    * @param xs Array of long values
    * @param i  First index for swapping
    * @param j  Second index for swapping
    * @throws `ArrayIndexOutOfBoundsException` if `i` or `j` aren't legal indices in `xs`
    *
    * @example {{{
    * scala> val ls = (0L to 3L).toArray
    * ls: Array[Long] = Array(0, 1, 2, 3)
    * scala> swapInPlace(ls, 0, 2)
    * scala> ls
    * res1: Array[Long] = Array(2, 1, 0, 3)
    * }}}
    */
  @inline def swapInPlace(ls: Array[Long], i: Int, j: Int): Unit = {
    ls(i) ^= ls(j)
    ls(j) ^= ls(i)
    ls(i) ^= ls(j)
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
  def rotateInPlace[@specialized(Boolean, Char, Double, Float, Int, Long) T](
    xs: Array[T],
    k: Int
  ): Unit = {
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
    // simple tests
    val bs = Array(true, false, true)
    swapInPlace(bs, 0, 1)
    sameElementsSameOrder(bs, Seq(false, true, true))
    val ds = Array(0D, 1D, 2D, 3D)
    swapInPlace(ds, 0, 2)
    sameElementsSameOrder(ds, Seq(2D, 1D, 0D, 3D))
    val fs = Array(0F, 1F, 2F, 3F)
    swapInPlace(fs, 0, 2)
    sameElementsSameOrder(fs, Seq(2F, 1F, 0F, 3F))
    val is = (0 to 3).toArray
    swapInPlace(is, 0, 2)
    sameElementsSameOrder(is, Seq(2, 1, 0, 3))
    val ls = (0L to 3L).toArray
    swapInPlace(ls, 0, 2)
    sameElementsSameOrder(ls, Seq(2L, 1L, 0L, 3L))
    // swapping is its own inverse
    val xs = ('A' to 'Z').toArray
    val ys = ('A' to 'Z').toSeq
    (0 to 100) foreach { _ =>
      val i = scala.util.Random.nextInt(xs.length)
      val j = scala.util.Random.nextInt(xs.length)
      swapInPlace(xs, i, j)
      swapInPlace(xs, i, j)
      sameElementsSameOrder(xs, ys)
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
