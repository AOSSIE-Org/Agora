package org.aossie.agora.votecounter

import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.model._

import spire.math.Rational

import scala.collection.mutable.{HashMap => MMap}

object ApprovalRule extends VoteCounter[Ballot]
  with SimpleApproval {

  override def winners(election: Election[Ballot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {

    val tls = countApprovals(election, ccandidates)
    tls.toList.sortWith( _._2 > _._2 ).take(numVacancies)
  }
}
