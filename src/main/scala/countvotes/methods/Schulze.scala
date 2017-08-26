package countvotes.methods

import countvotes.structures._

import scala.collection.mutable.ListBuffer

/**
  * Algorithm : https://en.wikipedia.org/wiki/Schulze_method
  */
object Schulze extends VoteCountingMethod[RankedWeightedBallot] {

  private val result: Result = new Result
  private val report: Report[RankedWeightedBallot] = new Report[RankedWeightedBallot]

  def runScrutiny(election: Election[RankedWeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[RankedWeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }


  override def winners(election: Election[RankedWeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val electionResponse = getPairwiseComparisonForWeightedElection(Election.rankedElectionToWeightedElection(election), ccandidates)

    schulzeWinnerRanking(getSchulzeStrongestPathMatrix(electionResponse, ccandidates), ccandidates).take(numVacancies)

  }

  // calculating the schulze response matrix using algorithm on https://en.m.wikipedia.org/wiki/Schulze_method
  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  def getSchulzeStrongestPathMatrix(electionResponse: Array[Array[Rational]], candidates: List[Candidate]): Array[Array[Rational]] = {

    val schulzeMatrix = Array.fill(candidates.size, candidates.size)(Rational(0, 1))
    val csize = candidates.size

    // initialize the schulze matrix
    for (i <- 0 until csize)
      for (j <- 0 until csize)
        if (i != j) {
          if (electionResponse(i)(j) > electionResponse(j)(i)) {
            schulzeMatrix(i)(j) = electionResponse(i)(j)
          } else {
            schulzeMatrix(i)(j) = 0
          }
        }

    // updating schulze matrix
    for (i <- 0 until csize)
      for (j <- 0 until csize)
        if (i != j)
          for (k <- 0 until csize)
            if (i != k && j != k) {
              schulzeMatrix(j)(k) = Math.max(schulzeMatrix(j)(k).toInt,
                Math.min(schulzeMatrix(j)(i).toInt, schulzeMatrix(i)(k).toInt))
            }

    schulzeMatrix
  }


  def schulzeWinnerRanking(schulzeMatrix: Array[Array[Rational]], candidates: List[Candidate]): List[(Candidate, Rational)] = {

    def better(candA: Candidate, candB: Candidate) = schulzeMatrix(candidates.indexOf(candA))(candidates.indexOf(candB)) > schulzeMatrix(candidates.indexOf(candB))(candidates.indexOf(candA))

    candidates sortWith {case (c1, c2) => better(c1, c2)} map (cand => (cand, Rational(0, 1)))

  }


}
