package countvotes.methods

import countvotes.structures._

import collection.mutable.{HashMap => Map}
import scala.collection.mutable


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

  override def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]

    for (b <- election if !b.preferences.isEmpty) {
      // need to take the size of the list first and then calculate the borda scores
      var bordaCounter = candidates.length

      b.preferences.filter(candidate => candidates.contains(candidate)).map(candidate => {
        m(candidate) = m.getOrElse(candidate, new Rational(0, 1)) + (bordaCounter - 1) * b.weight.numerator.toInt
        bordaCounter -= 1
      })
    }
    m
  }

  def average(totals: Map[Candidate, Rational], candidates: List[Candidate]): Rational = {
    var totalScore = Rational(0, 1)
    for (c <- candidates) {
      totalScore = totalScore + totals(c)
    }
    var averageScore = totalScore / Rational(candidates.length)
    averageScore
  }

  def winners(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {
    

    candidates.length match {

      case 1 => totals(election, candidates).toList

      case len if (len > 1) => {
        var tls = totals(election, candidates)
        var averageScore = average(tls,candidates)
        println(tls)
        println(averageScore)
        winners(election, candidates.filter(x => tls(x) > averageScore), numVacancies)
      }
    }
  }
}
