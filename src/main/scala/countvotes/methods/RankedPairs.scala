package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

object RankedPairs extends VoteCounter[RankBallot] with LazyLogging {

  def winners(e: Election[RankBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    logger.info("computing scored pairs winners: checking if the ballot contains the indtended data")

    List((ccandidates.head, Rational(0, 1)))
  }
}
