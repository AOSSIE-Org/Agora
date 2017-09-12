package agora.votecounter

import agora.votecounter.stv._
import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational
import agora.votecounter.stv.Input

class SimpleSTV extends STV[Ballot]
  with DroopQuota
  with NoFractionInQuota
  with NewWinnersNotOrdered[Ballot]
  with SimpleSurplusDistributionTieResolution // not necessary because of NewWinnersNotOrdered
  with SimpleExclusion
  with UnfairExclusionTieResolutuim
  with TransferValueWithDenominatorEqualToTotal
  with VoteCounterWithAllBallotsInSurplusDistribution
  with ExactWinnerRemoval{
  
  val result: Result = new Result
  val report: Report[Ballot] = new Report[Ballot]


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def runVoteCounter(election: Election[Ballot], candidates: List[Candidate], numVacancies: Int):   Report[Ballot]  = {
   val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
   println("Quota = " + quota)
   result.setQuota(quota)


   print("\n INPUT ELECTION: \n")
   //printElection(election)

   val tls = Election.firstVotes(election, candidates) // Here are totals of candidates also not OCCURING in the ballots
   result.addTotalsToHistory(tls)

   //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
   report.setCandidates(candidates)  // Here are candidates also not OCCURING in the election

   report.newCount(Input, None, Some(election), Some(tls), None, None)

   report.setWinners(winners(election, candidates, numVacancies))

   report
  }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(election: Election[Ballot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {

    println(" \n NEW RECURSIVE CALL \n")

    def mentionedCandidates[B <: PreferenceBallot](election: Election[B]): List[Candidate] = {
      val set = new collection.mutable.HashSet[Candidate]()
      for (b <- election) {
        for (c <- b.preferences)
          if (!set.exists(n => n == c) ) set += c
      }
      set.toList
    }
      
    val ccands = mentionedCandidates(election)

    val tls = Election.firstVotes(election, ccandidates)

    println("Totals: " + tls)

    if (ccands.length <= numVacancies){
      for (c <- ccands) yield (c, tls(c))
    }
    else {
      quotaReached(tls, result.getQuota) match {
        case true =>
          println("The quota is reached.")
          val ws: List[(Candidate, Rational)] = returnNewWinners(tls, result.getQuota)
          println("New winners: " + ws)
          result.addPendingWinners(ws.toList, None)

          val vacanciesFilled = ws.length >= numVacancies
          
          vacanciesFilled match {
              case false =>  {
                println("Vacancies are not yet filled.")
                val newElection = surplusesDistribution(election, numVacancies-ws.length)
                //printElection(newElection)
                winners(newElection, ccandidates.filterNot(ws.contains(_)), numVacancies-ws.length):::ws
                // TODO: care should be taken that newElection is not empty?!
              }
              case true => ws
            }
        case false =>
          val leastVotedCandidate = chooseCandidateForExclusion(tls)
          println("Excluding " + leastVotedCandidate)
          result.addExcludedCandidate(leastVotedCandidate._1, leastVotedCandidate._2)
          val newElection = exclusion(election, leastVotedCandidate._1, numVacancies)
          //printElection(newElection)
          winners(newElection, ccandidates.filterNot(x => x == leastVotedCandidate._1), numVacancies)
      }
    }
  }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def surplusesDistribution(election: Election[Ballot], numVacancies: Int): Election[Ballot] = {
  println("Distribution of surpluses.")
  var newElection = election
  while (result.getPendingWinners.nonEmpty){
   val (cand, ctotal, markings) = result.takeAndRemoveFirstPendingWinner
   newElection =  tryToDistributeSurplusVotes(newElection, cand, ctotal)
  }
  newElection
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 def tryToDistributeSurplusVotes(election: Election[Ballot], winner: Candidate, ctotal:Rational): Election[Ballot] = {

  val pendingWinners = result.getPendingWinners.map(x => x._1)

  if (ctotal == result.getQuota || !ballotsAreContinuing(winner, election, pendingWinners) )
   {
      removeWinnerWithoutSurplusFromElection(election, winner)
   }
  else {
    println("Distributing the surplus of " + winner)
    val surplus = ctotal - result.getQuota

    val tv = computeTransferValue(surplus, election, pendingWinners, winner, None)
    println("tv = " + tv)
    val res = distributeSurplusVotes(election, winner, ctotal, None, pendingWinners, tv)
    res._1
  }
 }

 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def exclusion(election: Election[Ballot], candidate: Candidate, numVacancies: Int): Election[Ballot] = {
   println("Exclusion of " + candidate)
   val ex = exclude(election, candidate, None, None)
   ex._1
 }



}
