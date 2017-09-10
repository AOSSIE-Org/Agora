package countvotes.methods

import countvotes.structures.{PreferenceBallot => Ballot}
import countvotes.algorithms._
import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

object ApprovalRule extends VoteCounter[Ballot]
  with SimpleApproval {

  override def winners(election: Election[Ballot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {

    val tls = countApprovals(election, ccandidates)
    tls.toList.sortWith( _._2 > _._2 ).take(numVacancies)
  }
}
