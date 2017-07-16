package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

/**
  * Algorithm via multiplication
  * http://www.alg.ewi.tudelft.nl/mates2010/media/matesslides/BrandtTournament%20Solutions%20(MATES).pdf
  * The Uncovered Set(UC) consists of all uncovered alternatives
  * x covers y (x C y) if D(y) is a subset of D(x)
  * where D(x) = { y ⍷ A | x >(majority) y}
  */
object UncoveredSetMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging{

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

    logger.info("Computing Uncovered Set")
    val zeroRational = Rational(0, 1)
    val majorityRational = Rational(1, 2)
    val electionResponse = getPairwiseComparison(e, ccandidates)
    val ucMatrix = BaseMatrix[Rational](ccandidates.size, ccandidates.size){ (i: Int, j: Int) => {
      zeroRational
    }
    }
    val totalVoters = Election.totalWeightedVoters(e)

    // mark all majority winners as rational 1
    ccandidates.foreach(c1 => {
      ccandidates.foreach(c2 => {
        if (ccandidates.indexOf(c1) < ccandidates.indexOf(c2)) {
          val pairscore = electionResponse(ccandidates.indexOf(c1))(ccandidates.indexOf(c2))
          if (pairscore > majorityRational*totalVoters) {
            ucMatrix(ccandidates.indexOf(c1))(ccandidates.indexOf(c2)) = Rational(1,1)
          } else {
            ucMatrix(ccandidates.indexOf(c1))(ccandidates.indexOf(c2)) = Rational(0,1)
          }
        }
      })
    })

    val uncoveredMatrix = addMatrix(addMatrix(square(ucMatrix, ccandidates.size), ucMatrix), identitiMatrix(ccandidates.size))

    uncoveredMatrix.zipWithIndex.filter(row => !row._1.contains(Rational(0, 1)))
      .map(row => (ccandidates(row._2), Rational(0, 1))).toList
  }

  def addMatrix(m1: Array[Array[Rational]], m2: Array[Array[Rational]]): Array[Array[Rational]] ={

    m1.zip(m2).map{rows: (Array[Rational], Array[Rational]) => {
      rows._1.zip( rows._2 ).map{ items:(Rational,Rational) =>
        items._1 + items._2
      }
    }}
  }

  def square(m: Array[Array[Rational]], size: Int): Array[Array[Rational]] = {
    val squredMatrix = for (m1 <- m) yield
      for (m2 <- transpose(m, size)) yield
        dotProduct(m1, m2)

    squredMatrix
  }

  def identitiMatrix(size: Int): Array[Array[Rational]] = BaseMatrix[Rational](size, size){(i: Int, j: Int) => {
    if (i == j) {
      Rational(1,1)
    } else {
      Rational(0, 1)
    }
  }}

  def dotProduct(row1: Array[Rational], row2: Array[Rational]): Rational = {
    row1.zip(row2).map{t: (Rational, Rational) => t._1 * t._2}.reduceLeft(_+_)
  }

  def transpose(matrix: Array[Array[Rational]], size: Int): Array[Array[Rational]] = {
    for (i <- 0 until size)
      for (j <- 0 until size)
        if (i<j) {
          val temp = matrix(i)(j)
          matrix(i)(j) = matrix(j)(i)
          matrix(i)(j) = temp
        }
    matrix
  }



}
