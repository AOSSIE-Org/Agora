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

    var tls = totals(election, candidates)

    result.addTotalsToHistory(tls)

    report.setCandidates(candidates)
    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    // here we need to apply random func on total voters not on election length as it might contain weights and this
    // rule is based on the probability of the voters favoring any candidate

    val zeroRational = Rational(0, 1)
    val totalVoters = getTotalVoters(election)
    val randomDictator = Random.nextInt(totalVoters.toInt + 1)

    var counter = 0

    election.filter(e => {
      counter += e.weight.toInt
      counter >= randomDictator
    }).take(1).map(e => (e.preferences)).flatMap(f => (f.map(c => (c, zeroRational))))
  }
}
