package countvotes.methods

import countvotes.structures._

import scala.util.Random

/**
  * Created by deepeshpandey on 03/06/17.
  * Algorithm : https://en.wikipedia.org/wiki/Random_ballot
  */
object RandomBallotMethod extends VoteCountingMethod[WeightedBallot] {

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

    randomBallotWinner(election, ccandidates, numVacancies, false)
  }

  def randomBallotWinner(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int,
                         useFixedSeed: Boolean): List[(Candidate, Rational)] = {

    // here we need to apply random func on total voters not on election length as it might contain weights and this
    // rule is based on the probability of the voters favoring any candidate

    val zeroRational = Rational(0, 1)
    val totalVoters = Election.totalWeightedVoters(election)
    val r = if (useFixedSeed) new Random(6142) else new Random()

    var randomDictator = r.nextInt(totalVoters.toInt + 1)

    // number of index to be dropped before reaching the random voter preference
    var index = 0
    for (e <- election if randomDictator > 0) {
      randomDictator -= e.weight.toInt
      if (randomDictator > 0) {
        index += 1
      }
    }

    election(index).preferences.take(numVacancies).map(c => (c, zeroRational))

  }
}
