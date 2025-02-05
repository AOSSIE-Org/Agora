package org.aossie.agora.votecounter

import org.aossie.agora.votecounter.stv._
import org.aossie.agora.model._

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Exhaustive_ballot */

object InstantExhaustiveBallot
    extends VoteCounter[PreferenceBallot]
    with SimpleExclusionWithFixedElectionSize {

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    val ct             = election.firstVotes(ccandidates)
    val sortedCandList = ct.toList.sortWith(_._2 < _._2)
    if (ct.size > 2) {
      val losingCand  = sortedCandList.head
      val newElection = exclude(election, losingCand._1)
      winners(newElection, ccandidates.filter(_ != losingCand._1), numVacancies)
    } else {
      sortedCandList.last :: List()
    }
  }

}
