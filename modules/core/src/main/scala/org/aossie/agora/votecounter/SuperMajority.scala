package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.votecounter.BipartisanSet.bipartisanSet
import spire.math.Rational

object SuperMajority extends VoteCounter[Ballot] {

  def runVoteCounter(election: Election[Ballot], candidates: List[Candidate], numVacancies: Int, param: Parameters):
  Report[Ballot] = {
    // print("\n INPUT ELECTION: \n")
    // //printElection(election)

    val result: Result = new Result
    val report: Report[Ballot] = new Report[Ballot]

    report.setCandidates(candidates)

    report.setWinners(superMajority(election, candidates, numVacancies, param))

    report
  }

  def superMajority(e: Election[Ballot], candidates: List[Candidate], numVacancies: Int, param: Parameters): List[(Candidate,Rational)] = {
    val sortedList = e.firstVotes(candidates).toList sortWith {
      (ct1, ct2) => ct1._2 > ct2._2
    }
    //Get majority percentage or default to 50%
    val percentage = param.majorityPercentage.getOrElse(0.5)
    if(percentage >= 0.5 && percentage <= 1.0) {
      sortedList.take(numVacancies).filter { case (c, r) => r > (e.length * percentage) }
    } else {
      //Default to 50% if specified percentage is less than 50% or greater than 100%
      sortedList.take(numVacancies).filter { case (c, r) => r > (e.length * 0.5) }
    }
  }
  override def winners(e: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = ???
}
