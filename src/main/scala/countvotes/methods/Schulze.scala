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

    val electionResponse = getPairwiseComaprisonForRankedElection(election, ccandidates)

    calculateSchulzeRankings(getSchulzeStrongestPathMatrix(electionResponse, ccandidates), ccandidates).take(numVacancies)

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

  // evaluating the schulze rankings from the schulze response matrix
  def calculateSchulzeRankings(schulzeResponseMatrix: Array[Array[Rational]],
                               ccandidates: List[Candidate]): List[(Candidate, Rational)] = {

    val schulzeRanking = new ListBuffer[(Candidate, Rational)]()
    val zeroRational = Rational(0,1)

    ccandidates.zipWithIndex.foreach(candidate => {
      schulzeRanking.size match {
        case 0 => schulzeRanking.insert(0, (candidate._1, zeroRational))
        case _ => {
          schulzeRanking.zipWithIndex.takeWhile(schulzeRanker => {
            if (schulzeResponseMatrix(candidate._2)(ccandidates.indexOf(schulzeRanker._1._1)) <
              schulzeResponseMatrix(ccandidates.indexOf(schulzeRanker._1._1))(candidate._2)) {
              schulzeRanking.insert(schulzeRanker._2, (candidate._1, zeroRational))
              false
            } else if (schulzeRanker._2 == schulzeRanking.length - 1) {
              schulzeRanking.insert(schulzeRanker._2 + 1, (candidate._1, zeroRational))
              false
            } else true
          })
        }
      }
    })
    schulzeRanking.toList.reverse
  }


}
