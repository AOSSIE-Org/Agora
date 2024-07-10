package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._

import spire.math.Rational

object RankedPairs extends VoteCounter[Candidate, RankBallot] with LazyLogging {

  def winners(
      e: Election[Candidate, RankBallot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {

    logger.info(
      "computing scored pairs winners: checking if the ballot contains the indtended data"
    )

    List((ccandidates.head, Rational(0, 1)))
  }

}
