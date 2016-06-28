package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._


object SimpleSTVMethod extends GenericSTVMethod[WeightedBallot]
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

  def runScrutiny(election: Election[WeightedBallot], numVacancies: Int):  Report[WeightedBallot]  = {  // all ballots of e are marked when the function is called
   val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
   println("Quota = " + quota)
   result.setQuota(quota)
         
 
   print("\n INPUT ELECTION: \n")
   printElection(election)
   
   val totals = computeTotals(election)
   result.addTotalsToHistory(totals) 
 
   report.setCandidates(getCandidates(election))
   report.newCount(FirstCount, None, Some(election), Some(totals), None)
   
   report.setWinners(computeWinners(election, numVacancies))   
   
   report   
  }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def computeWinners(election: Election[WeightedBallot], numVacancies: Int): List[(Candidate, Rational)] = {
    
    println(" \n NEW RECURSIVE CALL \n")
    
    val ccands = getCandidates(election)
       
    val totals = computeTotals(election)  
    println("Totals: " + totals)
    
    if (ccands.length <= numVacancies){
      for (c <- ccands) yield (c, totals(c))
    }
    else {  
      quotaReached(totals, result.getQuota) match {
        case true => 
          println("The quota is reached.")
          val winners: List[(Candidate, Rational)] = returnNewWinners(totals, result.getQuota) 
          println("New winners: " + winners)
          result.addPendingWinners(winners.toList, None) 
      
          vacanciesFilled(winners.length, numVacancies) match {
              case false =>  {
                println("Vacancies are not yet filled.")
                val newElection = surplusesDistribution(election, numVacancies-winners.length) 
                printElection(newElection)
                computeWinners(newElection, numVacancies-winners.length):::winners  // TODO: care should be taken that newElection is not empty?!
              }
              case true => winners
            } 
        case false =>  
          val leastVotedCandidate = chooseCandidateForExclusion(totals)
          println("Excluding " + leastVotedCandidate)
          result.addExcludedCandidate(leastVotedCandidate._1, leastVotedCandidate._2)
          val newElection = exclusion(election, leastVotedCandidate._1, numVacancies)
          printElection(newElection)
          computeWinners(newElection, numVacancies)
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
    distributeSurplusVotes(election, winner, ctotal, None, pendingWinners, tv)               
  }
 }
 
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def exclusion(election: Election[WeightedBallot], candidate: Candidate, numVacancies: Int): Election[WeightedBallot] = { 
   println("Exclusion of " + candidate)
   exclude(election, candidate, None, None) 
 }
  
 

}