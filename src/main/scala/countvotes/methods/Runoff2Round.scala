package countvotes.methods

import countvotes.methods.BordaRuleMethod.{printElection, totals, winners}
import countvotes.structures._
import collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 03/06/17.
  */
object Runoff2Round extends VoteCountingMethod[WeightedBallot] {

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

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val majorityRational = Rational(1, 2)
    val rnd1Winners = totals(election, ccandidates).toList.sortWith(_._2 > _._2).take(2)
    val totalVoters = getTotalVoters(election)
    if (rnd1Winners.head._2 > majorityRational * totalVoters)
      List(rnd1Winners.head)
    else
      getSecondRoundWinner(election, ccandidates, rnd1Winners.map(c => c._1), totalVoters.toInt, numVacancies)

  }

  def getSecondRoundWinner(election: Election[WeightedBallot], ccandidates: List[Candidate],
                           rnd1Winners: List[Candidate], totalVoters: Int, numVacancies: Int): List[(Candidate, Rational)] = {

    val m = new Map[Candidate, Rational]

    for (c <- ccandidates) m(c) = 0

    for (b <- election if b.preferences.nonEmpty) {
      val candidate = b.preferences.filter(c => rnd1Winners.contains(c)).take(1)
      m(candidate.head) = b.weight + m.getOrElse(candidate.head, 0)
    }

    m.toList.sortWith(_._2 > _._2).take(numVacancies)
  }
}
