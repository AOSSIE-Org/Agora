package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math.Rational

object ApprovalRule extends VoteCounter[Candidate, PreferenceBallot] with SimpleApproval {

  override def winners(
      election: Election[Candidate, PreferenceBallot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {

    val tls = countApprovals(election, ccandidates)
    tls.toList.sortWith(_._2 > _._2).take(numVacancies)
  }

}
