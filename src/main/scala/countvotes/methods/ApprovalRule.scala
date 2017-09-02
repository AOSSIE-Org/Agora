package countvotes.methods


import countvotes.algorithms._
import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

object ApprovalRule extends Scrutiny[WeightedBallot]
  with SimpleApproval {

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {

    val tls = countApprovals(election, ccandidates)
    tls.toList.sortWith( _._2 > _._2 ).take(numVacancies)
  }
}
