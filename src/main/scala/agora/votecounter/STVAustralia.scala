package agora.votecounter


import agora.model._

import scala.collection.mutable.{HashMap => Map}

import spire.math.Rational
import agora.votecounter.stv.Input
import agora.votecounter.stv.ACTBallot

abstract class STVAustralia extends STV[ACTBallot] {
  
  val result: Result = new Result
  val report: Report[ACTBallot] = new Report[ACTBallot]


 def tryToDistributeSurplusVotes(
   election: Election[ACTBallot], ccandidates: List[Candidate], winner: Candidate,
   ctotal:Rational, markings: Option[Set[Int]] ):
    (Election[ACTBallot], List[(Candidate,Rational)])

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 override def runVoteCounter(election: Election[ACTBallot], candidates: List[Candidate], numVacancies: Int):
    Report[ACTBallot] = {  // all ballots of e are marked when the function is called
   val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
   println("Number of ballots:" + election.length)
   println("Quota: " + quota)
   result.setQuota(quota)
   report.setQuota(quota)

   val tls = Election.totals(election, candidates) // Here are totals also of those candidates
                                          // that are NOT OCCURING in the ballots (i.e. when nobody mentioned them in preferences)
   result.addTotalsToHistory(tls)

   //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
   report.setCandidates(candidates)  // Here are totals also of those candidates that are NOT OCCURING in the ballots


   report.newCount(Input, None, Some(election), Some(tls), None, None)
   report.setLossByFractionToZero

   report.setWinners(winners(election, candidates, numVacancies))

   report
 }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ACT Legislation:
// 9(1): If a candidate is excluded in accordance with clause 8, the ballot papers counted for the candidate
// shall be sorted into groups according to their transfer values when counted for him or her.
//
 // like ACT
// Senate Legislation:
// (13AA)(a) and (13AA)(b)
//

  def determineStepsOfExclusion(election: Election[ACTBallot], candidate: Candidate): List[(Candidate, Rational)] = {
   var s: Set[(Candidate, Rational)] = Set()

   for (b <- election) {
      if (b.preferences.nonEmpty && b.preferences.head == candidate && !s.contains((candidate,b.value))) {
        s += ((candidate, b.value)) }
    }
   s.toList.sortBy(x => x._2).reverse //>
  }


}
