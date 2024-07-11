package org.aossie.agora.votecounter

import org.aossie.agora.model._
import collection.mutable.{HashMap => MMap}
import spire.math.Rational

trait SimpleApproval[C <: Candidate] {

  def countApprovals(
      election: Election[C, PreferenceBallot],
      candidates: List[C]
  ): MMap[C, Rational] = {
    val m = new MMap[C, Rational]
    for (b <- election)
      for (d <- b.preferences)
        m(d) = b.weight + (m.getOrElse(d, 0))
    m
  }

}
