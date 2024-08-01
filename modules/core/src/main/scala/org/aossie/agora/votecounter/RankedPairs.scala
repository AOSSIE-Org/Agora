package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._

import spire.math.Rational

object RankedPairs extends VoteCounter[RankBallot] with LazyLogging {

  def winners[C <: Candidate](
      e: Election[C, RankBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    logger.info(
      "computing scored pairs winners: checking if the ballot contains the indtended data"
    )

    List((ccandidates.head, Rational(0, 1)))
  }

}
