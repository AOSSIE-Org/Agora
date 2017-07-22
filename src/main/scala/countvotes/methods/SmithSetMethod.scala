package countvotes.methods

import countvotes.methods.InstantRunoff2Round.{printElection, totals, winners}
import countvotes.structures._

/**
  * Algorithm : Using Floyd Warshall Algorithm : http://wiki.electorama.com/wiki/Maximal_elements_algorithms
  */
object SmithSetMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }
  override def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val pairWiseComp = getPairwiseComparison(e, ccandidates)

    val relationMatrix = getRelationMatrix(e, ccandidates, pairWiseComp)

    val maximalSet = floydWarshallMaximal(relationMatrix, ccandidates)

    maximalSet.zip(ccandidates).filter {case (inMaximal, candidate) => inMaximal}
      .map{case(inMaximal, candidate) => (candidate, Rational(0, 1))}.toList

  }

  def getRelationMatrix(election: Election[WeightedBallot], ccandidates: List[Candidate], pairWiseComp: MatrixD2): Array[Array[Boolean]] = {

    val relationMatrix = BaseMatrix[Boolean](ccandidates.size, ccandidates.size){(i: Int, j: Int) => false}

    ccandidates.zipWithIndex.foreach(c1 => {
      ccandidates.zipWithIndex.foreach(c2 => {
        if (c1._2 != c2._2) {
          relationMatrix(c1._2)(c2._2) = pairWiseComp(c1._2)(c2._2) >= pairWiseComp(c2._2)(c1._2)
        }
      })
    })
    relationMatrix
  }

  def floydWarshallMaximal(relations: Array[Array[Boolean]], ccandidates: List[Candidate]): Array[Boolean] = {

    val isInMaximal = Array.ofDim[Boolean](ccandidates.size)
    for (i <- ccandidates.indices)
      isInMaximal(i) = true

    // eventually, hasPath[i][j] == true iff there is a path from i to j
    val hasPath = Array.ofDim[Boolean](ccandidates.size, ccandidates.size)
    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (i != j) {
            hasPath(i)(j) = relations(i)(j)
        }

    // expand consideration to paths that have intermediate nodes from 1 to k
    for (k <- ccandidates.indices)
      for (i <- ccandidates.indices)
        if (k != i) {
          for (j <- ccandidates.indices)
            if (k != j && i != j) {
              if (hasPath(i)(k) && hasPath(k)(j)) {
                hasPath(i)(j) = true
              }
            }
        }

    // disqualify as maximal any candidates that have paths to them
    // but no path back to complete a cycle

    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (i != j) {
          if (hasPath(j)(i) && !hasPath(i)(j)) {
            isInMaximal(i) = false
          }
        }

    isInMaximal

  }
}
