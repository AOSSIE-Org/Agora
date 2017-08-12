package countvotes.methods

import countvotes.structures._

import collection.mutable.{HashMap => Map}
import scala.collection.mutable
import countvotes.methods.BaldwinMethod.bordaScores

/**
  * https://en.wikipedia.org/wiki/Nanson%27s_method
  */
object NansonMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    var tls = totals(election, candidates)

    result.addTotalsToHistory(tls)

    report.setCandidates(candidates)
    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def winners(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {

    if (candidates.length == 1) {
      bordaScores(election, candidates).toList
    } else {
      var cbs = bordaScores(election, candidates) //borda scores of candidates
      val average = (Rational(0,1) /: candidates)(_ + cbs(_)) / Rational(candidates.length)
      winners(election, candidates.filter(x => cbs(x) > average), numVacancies)
    }
  }
}
