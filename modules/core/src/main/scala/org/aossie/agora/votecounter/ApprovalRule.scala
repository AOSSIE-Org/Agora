package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math.Rational

object ApprovalRule extends VoteCounter[PreferenceBallot] with SimpleApproval {

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    val tls = countApprovals(election, ccandidates)
    tls.toList.sortWith(_._2 > _._2).take(numVacancies)
  }

}
