package countvotes.methods

import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Range_voting
  * Note: This variant sums the score of candidates over all voters
  * TODO: Variant where average score of a candidate is used to compute the winner
  */
object RangeVoting extends VoteCountingMethod[ScoredWeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[ScoredWeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }


  def winners(election: Election[ScoredWeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val candidateScores = new MMap[Candidate, Rational]

    for (b <- election if b.preferences.nonEmpty) {
      b.scorePreferences.foreach(cscore => {
        candidateScores(cscore._1) = candidateScores.getOrElse(cscore._1, Rational(0, 1)) + cscore._2 * b.weight
      })
    }
    // because range voting is a single seat election - numVacancies are always 1
    List(candidateScores.toList.maxBy(_._2))
  }
}
