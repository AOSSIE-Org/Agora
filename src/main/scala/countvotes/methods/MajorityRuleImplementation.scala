package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

object MajorityRuleImplementation extends MajorityRule[WeightedBallot] {
  
  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {
      print("\n INPUT ELECTION: \n")
      printElection(election)
      
      var totals = computeTotals(election, candidates)
      result.addTotalsToHistory(totals)

      report.setCandidates(candidates) 
      report.newCount(Input, None, Some(election), Some(totals), None, None)
    
      report.setWinners(computeWinners(election, candidates, numVacancies))
      
      report
  }

  def computeWinners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ): 
  List[(Candidate,Rational)] = {
      require(numVacancies == 1, "Only one winner is possible")
      var reqMajority = Rational(1,2)
      val ccands = getCandidates(election)
      val totals = computeTotals(election, ccandidates)
      
      val numVoters: Int = election.length 
      for(c <- ccands if (reqMajority < Rational(totals(c).toInt, numVoters))) yield (c, totals(c))
  }
}

