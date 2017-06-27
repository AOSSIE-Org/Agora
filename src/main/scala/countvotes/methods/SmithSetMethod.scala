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

    val electionResponse = getPairwiseComparison(e, ccandidates)

    val relationMatrix = getRelationMatrix(e, ccandidates, electionResponse)

    val maximalSet = floydWarshallMaximal(relationMatrix, ccandidates)

    maximalSet.filter(p => p).zipWithIndex.map(p => (ccandidates(p._2), Rational(0, 1))).toList

  }

  def getRelationMatrix(election: Election[WeightedBallot], ccandidates: List[Candidate], eResponseMap: MatrixD2): Array[Array[Boolean]] = {

    val relationMatrix = BaseMatrix[Boolean](ccandidates.size, ccandidates.size){(i: Int, j: Int) => {
      false
    }}

    ccandidates.foreach(c1 => {
      ccandidates.foreach(c2 => {
        if (c1 != c2) {

          val c1c2PairValue = eResponseMap{ccandidates.indexOf(c1)}{ccandidates.indexOf(c2)}
          val c2c1PairValue = eResponseMap{ccandidates.indexOf(c2)}{ccandidates.indexOf(c1)}

          if (c1c2PairValue >= c2c1PairValue) {
            relationMatrix{ccandidates.indexOf(c1)}{ccandidates.indexOf(c2)} = true
          } else {
            relationMatrix{ccandidates.indexOf(c1)}{ccandidates.indexOf(c2)} = false
          }
        }
      })
    })
    relationMatrix
  }

  def floydWarshallMaximal(relations: Array[Array[Boolean]], ccandidates: List[Candidate]): Array[Boolean] = {

    val isInMaximal = Array.ofDim[Boolean](ccandidates.size)
    for (i <- ccandidates.indices)
      isInMaximal{i} = true

    // eventually, hasPath[i][j] == true iff there is a path from i to j
    val hasPath = Array.ofDim[Boolean](ccandidates.size, ccandidates.size)
    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (i != j) {
          if (relations{i}{j})
            hasPath{i}{j} = true
          else
            hasPath{i}{j} = false
        }

    // expand consideration to paths that have intermediate nodes from 1 to k
    for (k <- ccandidates.indices)
      for (i <- ccandidates.indices)
        if (k != i) {
          for (j <- ccandidates.indices)
            if (k != j && i != j) {
              if (hasPath{i}{k} && hasPath{k}{j}) {
                hasPath{i}{j} = true
              }
            }
        }

    // disqualify as maximal any candidates that have paths to them
    // but no path back to complete a cycle

    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (i != j) {
          if (hasPath{j}{i} && !hasPath{i}{j}) {
            isInMaximal{i} = false
          }
        }

    isInMaximal

  }
}
