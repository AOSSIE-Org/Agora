package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

object RankedPairsMethod extends Scrutiny[RankedWeightedBallot] with LazyLogging {

  def winners(e: Election[RankedWeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    logger.info("computing scored pairs winners: checking if the ballot contains the indtended data")

    List((ccandidates.head, Rational(0, 1)))
  }
}
