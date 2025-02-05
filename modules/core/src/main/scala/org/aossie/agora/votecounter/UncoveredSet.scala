package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._
import org.aossie.agora.votecounter.common.PreferencePairwiseComparison
import org.aossie.agora.util.matrix._

import scala.language.postfixOps

import spire.math.Rational
import org.aossie.agora.util.matrix.BaseMatrix

/** Algorithm via multiplication
  * http://www.alg.ewi.tudelft.nl/mates2010/media/matesslides/BrandtTournament%20Solutions%20(MATES).pdf
  * The Uncovered Set(UC) consists of all uncovered alternatives x covers y (x C y) if D(y) is a
  * subset of D(x) where D(x) = { y ⍷ A | x >(majority) y}
  */
object UncoveredSet
    extends VoteCounter[PreferenceBallot]
    with PreferencePairwiseComparison[PreferenceBallot]
    with LazyLogging {

  override def winners[C <: Candidate](
      e: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    logger.info("Computing Uncovered Set")
    val zeroRational     = Rational(0, 1)
    val majorityRational = Rational(1, 2)
    val electionResponse = pairwiseComparison(e, ccandidates)
    val ucMatrix = BaseMatrix[Rational](ccandidates.size, ccandidates.size) { (i: Int, j: Int) =>
      zeroRational
    }
    val totalVoters = e.weight

    // mark all majority winners as rational 1
    ccandidates.foreach(c1 => {
      ccandidates.foreach(c2 => {
        if (ccandidates.indexOf(c1) < ccandidates.indexOf(c2)) {
          val pairscore = electionResponse(ccandidates.indexOf(c1))(ccandidates.indexOf(c2))
          if (pairscore > majorityRational * totalVoters) {
            ucMatrix(ccandidates.indexOf(c1))(ccandidates.indexOf(c2)) = Rational(1, 1)
          } else {
            ucMatrix(ccandidates.indexOf(c1))(ccandidates.indexOf(c2)) = Rational(0, 1)
          }
        }
      })
    })

    // matrix calculation step from algorithm
    val uncoveredMatrix = addMatrix(
      addMatrix(square(ucMatrix, ccandidates.size), ucMatrix),
      identityMatrix(ccandidates.size)
    )

    uncoveredMatrix
      .zip(ccandidates)
      .filter { case (row, candidate) =>
        !row.contains(Rational(0, 1))
      }
      .map { case (row, candidate) =>
        (candidate, Rational(0, 1))
      } toList

  }

}
