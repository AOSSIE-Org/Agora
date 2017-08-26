package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._
import countvotes.util.matrix._

import scala.language.postfixOps


/**
  * Algorithm via multiplication
  * http://www.alg.ewi.tudelft.nl/mates2010/media/matesslides/BrandtTournament%20Solutions%20(MATES).pdf
  * The Uncovered Set(UC) consists of all uncovered alternatives
  * x covers y (x C y) if D(y) is a subset of D(x)
  * where D(x) = { y ⍷ A | x >(majority) y}
  */
object UncoveredSetMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging {

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
    val electionResponse = getPairwiseComparisonForWeightedElection(e, ccandidates)
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

    // matrix calculation step from algorithm
    val uncoveredMatrix = addMatrix(addMatrix(square(ucMatrix, ccandidates.size), ucMatrix), identityMatrix(ccandidates.size))

    (uncoveredMatrix zip ccandidates) filter {
      case (row, candidate) => !row.contains(Rational(0, 1))} map {
      case (row, candidate) => (candidate, Rational(0, 1))} toList

  }

}
