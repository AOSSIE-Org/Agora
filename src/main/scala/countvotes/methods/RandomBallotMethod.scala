package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

import scala.util.Random

/**
  * Algorithm : https://en.wikipedia.org/wiki/Random_ballot
  */
object RandomBallotMethod extends VoteCounter[Ballot] with LazyLogging {

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    randomBallotWinner(election, ccandidates, numVacancies, None)
  }

  def randomBallotWinner(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int,
                         seed: Option[Int]): List[(Candidate, Rational)] = {

    logger.info("computing random ballot winner")

    val totalVoters = Election.totalWeightedVoters(election)

    val random = seed match {
      case Some(seed) => new Random(seed).nextDouble()
      case None => new Random().nextDouble()
    }

    val cumulative = election.map(_.weight).scanLeft(Rational(0, 1))(_ + _)

    val dictator = cumulative.lastIndexWhere(_.toDouble <= random * totalVoters.toDouble)

    election(dictator).preferences.take(numVacancies).map(c => (c, Rational(0, 1)))

  }
}
