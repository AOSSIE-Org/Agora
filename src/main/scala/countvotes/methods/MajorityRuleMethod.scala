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

  // TODO: There is an implicit assumption here that all votes have weight 1.
  // Should this be checked?
  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
      totals(election, ccandidates).toList sortWith {
        (ct1, ct2) => ct1._2 > ct2._2
      } take(numVacancies) filter { case (c, t) => t > (election.length / 2) } // only select the alternative that has more than half the votes
  }
}

