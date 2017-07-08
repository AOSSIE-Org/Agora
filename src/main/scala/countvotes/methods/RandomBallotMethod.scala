package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

import scala.util.Random

/**
  * Algorithm : https://en.wikipedia.org/wiki/Random_ballot
  */
object RandomBallotMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report

  }

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    randomBallotWinner(election, ccandidates, numVacancies, None)
  }

  def randomBallotWinner(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int,
                         seed: Option[Int]): List[(Candidate, Rational)] = {

    logger.info("computing random ballot winner")

    val totalVoters = Election.totalWeightedVoters(election)

    val random = seed match {
      case Some(seed) => new Random(seed).nextInt(totalVoters.toInt)
      case None => new Random().nextInt(totalVoters.toInt)
    }

    val cumulative = election.map(_.weight).scanLeft(Rational(0, 1))(_ + _)

    val dictator = cumulative.lastIndexWhere(_ <= random)

    election(dictator).preferences.take(numVacancies).map(c => (c, Rational(0, 1)))

  }
}
