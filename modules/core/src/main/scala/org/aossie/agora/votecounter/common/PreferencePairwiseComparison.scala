package org.aossie.agora.votecounter.common

import org.aossie.agora.model._
import spire.math.Rational

trait PreferencePairwiseComparison[
    C <: Candidate,
    B[CC >: C <: Candidate] <: PreferenceBallot[CC]
] {

  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def pairwiseComparison(
      election: Election[C, B],
      candidates: List[C]
  ): Array[Array[Rational]] = {
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))

    for (b <- election) {
      val pi = b.preferences.zipWithIndex
      for {
        (c1, i1) <- pi
        (c2, i2) <- pi.take(i1)
      }
        responseMatrix(candidates.indexOf(c2))(candidates.indexOf(c1)) += b.weight
    }

    responseMatrix
  }

}
