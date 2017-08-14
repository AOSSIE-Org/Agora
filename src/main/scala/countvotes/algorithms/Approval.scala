package countvotes.algorithms


import countvotes.structures._
import countvotes.methods._

import collection.mutable.{HashMap => MMap}

trait SimpleApproval {
  def countApprovals(election: Election[WeightedBallot], candidates: List[Candidate]): MMap[Candidate, Rational] = {
    val m = new MMap[Candidate, Rational]
    for (b <- election if !b.preferences.isEmpty) {
      for(d <- b.preferences) {
        m(d) = b.weight + (m.getOrElse(d, 0))
      }
    }
    m
  }
}