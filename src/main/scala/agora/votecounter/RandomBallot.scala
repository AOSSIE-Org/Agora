package agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import scala.util.Random

import spire.math.Rational

/**
  * Algorithm : https://en.wikipedia.org/wiki/Random_ballot
  */
object RandomBallot extends VoteCounter[Ballot] with LazyLogging {

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    randomBallotWinner(election, ccandidates, numVacancies, None)
  }

  def randomBallotWinner(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int,
                         seed: Option[Int]): List[(Candidate, Rational)] = {

    logger.info("computing random ballot winner")

    val totalVoters = election.totalWeightedVoters

    val random = seed match {
      case Some(seed) => new Random(seed).nextDouble()
      case None => new Random().nextDouble()
    }

    val cumulative = election.map(_.weight).scanLeft(Rational(0, 1))(_ + _)

    val dictator = cumulative.lastIndexWhere(_.toDouble <= random * totalVoters.toDouble)

    election(dictator).preferences.take(numVacancies).map(c => (c, Rational(0, 1)))

  }
}
