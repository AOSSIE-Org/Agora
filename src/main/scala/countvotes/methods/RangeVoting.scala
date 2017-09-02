package countvotes.methods

import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Range_voting
  * Note: This variant sums the score of candidates over all voters
  * TODO: Variant where average score of a candidate is used to compute the winner
  */
object RangeVoting extends Scrutiny[ScoredWeightedBallot] {

  def winners(election: Election[ScoredWeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val candidateScores = new MMap[Candidate, Rational]

    for (b <- election if b.preferences.nonEmpty) {
      b.scorePreferences.foreach { case(candidate, score) => {
        candidateScores(candidate) = candidateScores.getOrElse(candidate, Rational(0, 1)) + score * b.weight
      }}
    }
    // because range voting is a single seat election - numVacancies are always 1
    List(candidateScores.toList.maxBy(_._2))
  }
}
