package agora.util

import agora.util.matrix.BaseMatrix

import spire.math.Rational

/** Created by deepeshpandey on 30/07/17. */
package object matrix {

  def addMatrix(m1: Array[Array[Rational]], m2: Array[Array[Rational]]): Array[Array[Rational]] = {

    m1.zip(m2).map { rows: (Array[Rational], Array[Rational]) =>
      {
        rows._1.zip(rows._2).map { items: (Rational, Rational) =>
          items._1 + items._2
        }
      }
    }
  }

  def square(m: Array[Array[Rational]], size: Int): Array[Array[Rational]] = {
    val squaredMatrix = for (m1 <- m) yield for (m2 <- transpose(m, size)) yield dotProduct(m1, m2)

    squaredMatrix
  }

  def identityMatrix(size: Int): Array[Array[Rational]] = BaseMatrix[Rational](size, size) {
    (i: Int, j: Int) =>
      {
        if (i == j) {
          Rational(1, 1)
        } else {
          Rational(0, 1)
        }
      }
  }

  def dotProduct(row1: Array[Rational], row2: Array[Rational]): Rational =
    row1.zip(row2).map { t: (Rational, Rational) => t._1 * t._2 }.reduce(_ + _)

  def transpose(matrix: Array[Array[Rational]], size: Int): Array[Array[Rational]] = {
    for (i <- 0 until size)
      for (j <- 0 until size)
        if (i < j) {
          val temp = matrix(i)(j)
          matrix(i)(j) = matrix(j)(i)
          matrix(i)(j) = temp
        }
    matrix
  }

}
