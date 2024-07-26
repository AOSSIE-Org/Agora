package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.votecounter.common.PreferencePairwiseComparison

import scala.language.postfixOps

import spire.math.Rational
import org.aossie.agora.util.matrix.BaseMatrix

/** Algorithm : http://wiki.electorama.com/wiki/Maximal_elements_algorithms#Floyd-Warshall_algorithm
  */
object SmithSet extends VoteCounter[PreferenceBallot] with PreferencePairwiseComparison {

  override def winners[C <: Candidate](
      e: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    val pairWiseComp = pairwiseComparison(e, ccandidates)

    val relationMatrix = getRelationMatrix(e, ccandidates, pairWiseComp)

    val maximalArray = floydWarshallMaximal(relationMatrix, ccandidates)

    maximalArray
      .zip(ccandidates)
      .filter { case (inMaximal, candidate) =>
        inMaximal
      }
      .map { case (inMaximal, candidate) =>
        (candidate, Rational(0, 1))
      } toList

  }

  /** get the relation matrix for the algorithm as described in
    * http://wiki.electorama.com/wiki/Maximal_elements_algorithms#Background
    * @param election
    * @param ccandidates
    * @param pairWiseComp
    * @return
    */
  def getRelationMatrix[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      pairWiseComp: Array[Array[Rational]]
  ): Array[Array[Boolean]] = {

    val relationMatrix = BaseMatrix[Boolean](ccandidates.size, ccandidates.size) {
      (i: Int, j: Int) => false
    }

    ccandidates.zipWithIndex.foreach(c1 => {
      ccandidates.zipWithIndex.foreach(c2 => {
        if (c1._2 != c2._2) {
          relationMatrix(c1._2)(c2._2) = pairWiseComp(c1._2)(c2._2) >= pairWiseComp(c2._2)(c1._2)
        }
      })
    })
    relationMatrix
  }

  // scalastyle:off cyclomatic.complexity
  def floydWarshallMaximal(
      relations: Array[Array[Boolean]],
      ccandidates: List[Candidate]
  ): Array[Boolean] = {

    val isInMaximal = Array.ofDim[Boolean](ccandidates.size)
    for (i <- ccandidates.indices)
      isInMaximal(i) = true

    // eventually, hasPath[i][j] == true iff there is a path from i to j
    val hasPath = Array.ofDim[Boolean](ccandidates.size, ccandidates.size)
    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (i != j) {
          hasPath(i)(j) = relations(i)(j)
        }

    // expand consideration to paths that have intermediate nodes from 1 to k
    for (k <- ccandidates.indices)
      for (i <- ccandidates.indices)
        if (k != i) {
          for (j <- ccandidates.indices)
            if (k != j && i != j) {
              if (hasPath(i)(k) && hasPath(k)(j)) {
                hasPath(i)(j) = true
              }
            }
        }

    // disqualify as maximal any candidates that have paths to them
    // but no path back to complete a cycle

    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (i != j) {
          if (hasPath(j)(i) && !hasPath(i)(j)) {
            isInMaximal(i) = false
          }
        }

    isInMaximal

  }

}
