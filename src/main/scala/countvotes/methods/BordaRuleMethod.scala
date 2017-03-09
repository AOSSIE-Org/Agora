package countvotes.methods

import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}
import collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 07/03/17.
  */
object BordaRuleMethod extends VoteCountingMethod[WeightedBallot]{

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

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

  override def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]
    val totalCandidates = getCandidates(election).length

    for (b <- election if !b.preferences.isEmpty) {
      // need to take the size of the list first and then calculate the borda scores
      var bordaCounter = totalCandidates

      b.preferences.map(x => {
        m(x) = m.getOrElse(x, new Rational(0,1)) + (bordaCounter-1)
        bordaCounter -= 1
      })

    }
    m
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {

    val tls = totals(election, ccandidates)

    tls.toList.sortWith(_._2 > _._2)

  }

}
