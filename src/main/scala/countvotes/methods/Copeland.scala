package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Copeland%27s_method
  */
object Copeland extends VoteCounter[Ballot] with LazyLogging {

  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    logger.info("Computing Copeland winner")

    val candidatesNetScores = new MMap[Candidate, Rational]
    val majorityRational = Rational(1, 2)
    val totalVoters = Election.totalWeightedVoters(election)

    // calculate pariwise comparison according to the ranked voting methods
    // non - ranked candidates are worse than ranked candidates

    val pairwiseComp = Array.fill(ccandidates.size, ccandidates.size)(Rational(0, 1))

    for (b <- election if b.preferences.nonEmpty) {
      for (c1 <- b.preferences; c2 <- ccandidates) {
        if (!b.preferences.contains(c2) || b.preferences.indexOf(c1) < b.preferences.indexOf(c2)) {
          pairwiseComp(ccandidates.indexOf(c1))(ccandidates.indexOf(c2)) += b.weight
        }
      }
    }

    // calculate the net victory for each candidate and find the copeland winner
    for (c1 <- ccandidates.indices; c2 <- ccandidates.indices   if c1 < c2) {

      if (pairwiseComp(c1)(c2) > majorityRational * totalVoters) {

        candidatesNetScores(ccandidates(c1)) = candidatesNetScores.getOrElse(ccandidates(c1), Rational(0,1)) + 1
        candidatesNetScores(ccandidates(c2)) = candidatesNetScores.getOrElse(ccandidates(c2), Rational(0,1)) - 1

      } else if (pairwiseComp(c2)(c1) > majorityRational * totalVoters) {

        candidatesNetScores(ccandidates(c2)) = candidatesNetScores.getOrElse(ccandidates(c2), Rational(0,1)) + 1
        candidatesNetScores(ccandidates(c1)) = candidatesNetScores.getOrElse(ccandidates(c1), Rational(0,1)) - 1

      }
    }
    candidatesNetScores.toList.sortWith(_._2 > _._2).take(numVacancies)
  }
}
