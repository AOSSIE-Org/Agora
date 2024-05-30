package org.aossie.agora.votecounter

import spire.math.Rational

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => MMap}

/** * https://en.wikipedia.org/wiki/Satisfaction_approval_voting
  */

object SatisfactionApprovalVoting extends VoteCounter[Ballot] {

  def winners(
      election: Election[Ballot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {
    // following code makes use of additive satisfcation property of Satisfaction Approval Voting
    val candidateScoreMap = new MMap[Candidate, Rational]
    for (b <- election if !b.preferences.isEmpty)
      for (c <- b.preferences)
        candidateScoreMap(c) = Rational(1, b.preferences.size) + candidateScoreMap.getOrElse(c, 0)
    candidateScoreMap.toList.sortWith(_._2 > _._2).take(numVacancies)
  }

}
