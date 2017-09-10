package countvotes.algorithms


import countvotes.structures.{PreferenceBallot => Ballot, _}
import countvotes.methods._

import collection.mutable.{HashMap => MMap}

import spire.math.Rational

trait SimpleApproval {
  def countApprovals(election: Election[Ballot], candidates: List[Candidate]): MMap[Candidate, Rational] = {
    val m = new MMap[Candidate, Rational]
    for (b <- election) {
      for(d <- b.preferences) {
        m(d) = b.weight + (m.getOrElse(d, 0))
      }
    }
    m
  }
}