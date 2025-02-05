package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._

import scala.util.Random

import spire.math.Rational

/** Algorithm : https://en.wikipedia.org/wiki/Random_ballot */
object RandomBallot extends VoteCounter[PreferenceBallot] with LazyLogging {

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] =
    randomBallotWinner(election, ccandidates, numVacancies, None)

  def randomBallotWinner[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int,
      seed: Option[Int]
  ): List[(C, Rational)] = {

    logger.info("computing random ballot winner")

    val totalVoters = election.weight

    val random = seed match {
      case Some(seed) => new Random(seed).nextDouble()
      case None       => new Random().nextDouble()
    }

    val cumulative = election.map(_.weight).scanLeft(Rational(0, 1))(_ + _)

    val dictator = cumulative.lastIndexWhere(_.toDouble <= random * totalVoters.toDouble)

    election(dictator).preferences.take(numVacancies).map(c => (c, Rational(0, 1)))

  }

}
