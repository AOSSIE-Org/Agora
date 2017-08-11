package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

object RankedPairsMethod extends VoteCountingMethod[WeightedScoreRankBallot] with LazyLogging {

  private val result: Result = new Result
  private val report: Report[WeightedScoreRankBallot] = new Report[WeightedScoreRankBallot]

  def runScrutiny(election: Election[WeightedScoreRankBallot], candidates: List[Candidate],
                  numVacancies: Int): Report[WeightedScoreRankBallot] = {

    print("\n INPUT ELECTION: \n")

    report.setCandidates(candidates)

    winners(election, candidates, numVacancies)

    report
  }

  def winners(e: Election[WeightedScoreRankBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    logger.info("computing scored pairs winners: checking if the ballot contains the indtended data")

    List((ccandidates.head, Rational(0, 1)))
  }
}
