package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

object MajorityRuleMethod extends MajorityRule[WeightedBallot] {
  
  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {
      print("\n INPUT ELECTION: \n")
      printElection(election)
      
      var tls = totals(election, candidates)
      result.addTotalsToHistory(tls)

      report.setCandidates(candidates) 
      report.newCount(Input, None, Some(election), Some(tls), None, None)
    
      report.setWinners(winners(election, candidates, numVacancies))
      
      report
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ): 
  List[(Candidate,Rational)] = {
      require(numVacancies == 1, "Only one winner is possible in Majority rule")
      var reqMajority = Rational(1,2)
      val ccands = getCandidates(election)
      val tls = totals(election, ccandidates)
      
      val numVoters: Int = election.length 
      for(c <- ccands if (reqMajority < Rational(tls(c).toInt, numVoters))) yield (c, tls(c))
  }
}

