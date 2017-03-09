package countvotes.methods

import countvotes.structures._

import collection.mutable.{HashMap => Map}
import scala.collection.mutable

/**
  * Created by deepeshpandey on 09/03/17.
  */
object NansonRuleMethod extends VoteCountingMethod[WeightedBallot] {

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

  override def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]

    for (b <- election if !b.preferences.isEmpty) {
      // need to take the size of the list first and then calculate the borda scores
      var bordaCounter = candidates.length

      b.preferences.filter(candidate => candidates.contains(candidate)).map(candidate => {
        m(candidate) = m.getOrElse(candidate, new Rational(0, 1)) + (bordaCounter - 1)
        bordaCounter -= 1
      })

    }
    m
  }

  def winners(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = candidates.length match {

    case 1 => totals(election, candidates).toList

    case len if (len > 1) => {
      // removing the lowest borda score candidate from the candidate list
      var lowestBordaCandidate = totals(election, candidates).minBy(_._2)
      println("Removed lowest score borda candidate " + lowestBordaCandidate)
      winners(election, candidates.filter(_ != lowestBordaCandidate._1), numVacancies)
    }
  }

}
