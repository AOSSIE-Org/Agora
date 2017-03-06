package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

object FirstPastThePostMethod extends FirstPastThePost[WeightedBallot] {

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {
      print("\n INPUT ELECTION: \n")
      printElection(election)

      var totals = computeTotals(election, candidates)
      result.addTotalsToHistory(totals)

      report.setCandidates(candidates)
      report.newCount(Input, None, Some(election), Some(totals), None, None)

      report.setWinners(winners(election, candidates, numVacancies))

      report
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
      require(numVacancies == 1, "Only one winner is possible in First past the post rule")
      var ccands = getCandidates(election)
      val tls = totals(election, ccandidates)
      var cand= ccands(0)

      for(c <- ccands if (totals(cand) < tls(c))){
        cand=c
      }
      return (cand,tls(cand))
  }
}
