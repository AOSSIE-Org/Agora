package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._


class SimpleSTVMethod extends STV[WeightedBallot]
  with DroopQuota
  with NoFractionInQuota
  with NewWinnersNotOrdered[WeightedBallot]
  with SimpleSurplusDistributionTieResolution // not necessary because of NewWinnersNotOrdered
  with SimpleExclusion
  with UnfairExclusionTieResolutuim
  with TransferValueWithDenominatorEqualToTotal
  with ScrutinyWithAllBallotsInSurplusDistribution
  with ExactWinnerRemoval{


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {  
   val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
   println("Quota = " + quota)
   result.setQuota(quota)


   print("\n INPUT ELECTION: \n")
   printElection(election)
   
   val totals = computeTotals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots
   result.addTotalsToHistory(totals) 
 
   //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
   report.setCandidates(candidates)  // Here are candidates also not OCCURING in the election
   
   report.newCount(Input, None, Some(election), Some(totals), None, None)
   
   report.setWinners(winners(election, candidates, numVacancies))   
   
   report   
  }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {
    
    println(" \n NEW RECURSIVE CALL \n")

    val ccands = getCandidates(election)
       
    val totals = computeTotals(election, ccandidates)  

    println("Totals: " + totals)

    if (ccands.length <= numVacancies){
      for (c <- ccands) yield (c, totals(c))
    }
    else {
      quotaReached(totals, result.getQuota) match {
        case true =>
          println("The quota is reached.")
          val ws: List[(Candidate, Rational)] = returnNewWinners(totals, result.getQuota)
          println("New winners: " + ws)
          result.addPendingWinners(ws.toList, None)

          vacanciesFilled(ws.length, numVacancies) match {
              case false =>  {
                println("Vacancies are not yet filled.")
                val newElection = surplusesDistribution(election, numVacancies-ws.length)
                printElection(newElection)
                winners(newElection, ccandidates.filterNot(ws.contains(_)), numVacancies-ws.length):::ws  // TODO: care should be taken that newElection is not empty?!
              }
              case true => ws
            }
        case false =>
          val leastVotedCandidate = chooseCandidateForExclusion(totals)
          println("Excluding " + leastVotedCandidate)
          result.addExcludedCandidate(leastVotedCandidate._1, leastVotedCandidate._2)
          val newElection = exclusion(election, leastVotedCandidate._1, numVacancies)
          printElection(newElection)
          winners(newElection, ccandidates.filterNot(x => x == leastVotedCandidate._1), numVacancies)
      }
    }
  }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def surplusesDistribution(election: Election[WeightedBallot], numVacancies: Int): Election[WeightedBallot] = {
  println("Distribution of surpluses.")
  var newElection = election
  while (result.getPendingWinners.nonEmpty){
   val (cand, ctotal, markings) = result.takeAndRemoveFirstPendingWinner
   newElection =  tryToDistributeSurplusVotes(newElection, cand, ctotal)
  }
  newElection
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 def tryToDistributeSurplusVotes(election: Election[WeightedBallot], winner: Candidate, ctotal:Rational): Election[WeightedBallot] = {

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

 def exclusion(election: Election[WeightedBallot], candidate: Candidate, numVacancies: Int): Election[WeightedBallot] = {
   println("Exclusion of " + candidate)
   val ex = exclude(election, candidate, None, None)
   ex._1
 }



}
