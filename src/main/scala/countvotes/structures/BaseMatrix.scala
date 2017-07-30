package countvotes.structures

/**
  * 2 Dimensional matrix
  */
object BaseMatrix {

  def apply(rows: Int, cols: Int)(f: (Int, Int) => Rational): Array[Array[Rational]] =  {

    val matrix = Array.ofDim[Rational](rows, cols)
    for (i <- 0 until rows)
      for (j <- 0 until cols)
        matrix{i}{j} = f(i,j)

    matrix
  }
}