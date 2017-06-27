package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._
import collection.mutable.{HashMap => MMap}

/**
  * Algorithm : https://en.wikipedia.org/wiki/Copeland%27s_method
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

    val electionResponse = getPairwiseComparison(e, ccandidates)
    val candidatesNetScores = new MMap[Candidate, Rational]
    val majorityRational = Rational(1, 2)
    val totalVoters = Election.totalWeightedVoters(e)

    // calculate the net victory for each candidate and find the copeland winner

    ccandidates.foreach(c1 => {
      ccandidates.foreach(c2 => {
        if (ccandidates.indexOf(c2) > ccandidates.indexOf(c1)) {
          val pairScore = electionResponse{ccandidates.indexOf(c1)}{ccandidates.indexOf(c2)}
          if (pairScore > majorityRational * totalVoters) {
            candidatesNetScores(c1) = candidatesNetScores.getOrElse(c1, Rational(0,1)) + 1
            candidatesNetScores(c2) = candidatesNetScores.getOrElse(c2, Rational(0,1)) - 1
          } else {
            if (pairScore == majorityRational * totalVoters) {
              candidatesNetScores(c1) = candidatesNetScores.getOrElse(c1, Rational(0,1)) + Rational(1, 2)
              candidatesNetScores(c2) = candidatesNetScores.getOrElse(c2, Rational(0,1)) + Rational(1, 2)
            } else {
              candidatesNetScores(c2) = candidatesNetScores.getOrElse(c2, Rational(0,1)) + 1
              candidatesNetScores(c1) = candidatesNetScores.getOrElse(c1, Rational(0,1)) - 1
            }
          }
        }
      })
    })
    candidatesNetScores.toList.sortWith(_._2 > _._2).take(numVacancies)
  }
}
