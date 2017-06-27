
package countvotes.structures

/**
  * 2 Dimensional matrix
  */
object BaseMatrix {

  def apply[T: Manifest](rows: Int, cols: Int)(f: (Int, Int) => T): Array[Array[T]] =  {

    val matrix = Array.ofDim[T](rows, cols)
    for (i <- 0 until rows)
      for (j <- 0 until cols)
        matrix{i}{j} = f(i,j)

    matrix
  }
}