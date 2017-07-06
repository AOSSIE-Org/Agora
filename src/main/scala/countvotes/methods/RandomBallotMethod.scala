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
                         useFixedSeed: Option[Int]): List[(Candidate, Rational)] = {

    logger.info("computing random ballot winner")

    val totalVoters = Election.totalWeightedVoters(election)

    val random = useFixedSeed match {
      case Some(seed) => new Random(seed).nextInt(totalVoters.toInt + 1)
      case None => new Random().nextInt(totalVoters.toInt + 1)
    }

    val cumulative = election.map(_.weight).scanLeft(Rational(0, 1))(_ + _)

    val dictator = cumulative.indexWhere(_ >= random)

    // case when random generator gives 0 as an output - one of the test case
    if (dictator == 0) List()
    else election(dictator-1).preferences.take(numVacancies).map(c => (c, Rational(0, 1)))

  }
}
