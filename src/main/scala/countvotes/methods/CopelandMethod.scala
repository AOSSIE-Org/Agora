package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

/**
  * Created by deepeshpandey on 26/06/17.
  */
object CopelandMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    logger.info("Computing Copeland winner")

    val electionResponse =


    ???
  }
}
