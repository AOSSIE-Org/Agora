package org.aossie.agora.votecounter.common

import org.aossie.agora.model._
import spire.math.Rational

trait RankPairwiseComparison {

  // utility method for matrix where a[i][j] = x means candidate i has got #x votes against candidate j
  def pairwiseComparison[C <: Candidate, B[CC >: C <: Candidate] <: RankBallot[CC]](
      election: Election[C, B],
      candidates: List[C]
  ): Array[Array[Rational]] = {
    val responseMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))

    for (b <- election) {
      val pi = b.sortedRanks.zipWithIndex
      for {
        ((c1, s1), i1) <- pi
        ((c2, s2), i2) <- pi.take(i1) if s1 != s2
      }
        responseMatrix(candidates.indexOf(c2))(candidates.indexOf(c1)) += b.weight
    }

    responseMatrix
  }

}
