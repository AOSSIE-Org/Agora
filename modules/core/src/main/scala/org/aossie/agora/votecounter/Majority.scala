package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math.Rational

object Majority extends VoteCounter[PreferenceBallot] {

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      candidate: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    // TODO: There is an implicit assumption here that all votes have weight 1.
    // Should this be checked?
    election
      .firstVotes(candidate)
      .toList
      .sortWith { (ct1, ct2) =>
        ct1._2 > ct2._2
      }
      .take(numVacancies)
      .filter { case (c, t) =>
        t > (election.length / 2)
      } // only select the alternative that has more than half the votes
  }

}
