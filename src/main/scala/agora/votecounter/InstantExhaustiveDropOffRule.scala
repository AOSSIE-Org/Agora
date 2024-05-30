package agora.votecounter

import agora.votecounter.stv._
import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Exhaustive_ballot#Notes */

object InstantExhaustiveDropOffRule
    extends VoteCounter[Ballot]
    with SimpleExclusionWithFixedElectionSize {

  var dropOffPercentage = Rational(0, 100)

  val cutoffPercentage =
    25 // drop off of candidates in steps of 5% till 25% after which lowest scoring candidate is eliminated

  def loser(
      candidate: (Candidate, Rational),
      total: Int,
      dropOffPercentage: Rational
  ): Option[(Candidate, Rational)] = {
    if (dropOffPercentage.numerator * (100 / dropOffPercentage.denominator) > cutoffPercentage) {
      // println(" 25% dropoff rule crossed, eliminating the last candidate " + candidate._1)
      Some(candidate)
    } else {
      val ccandPercentage = Rational(candidate._2.numerator, total)
      if (ccandPercentage < dropOffPercentage) {
        // println(" Dropping off candidate " + candidate._1 + " for being below " + dropOffPercentage.numerator*100/dropOffPercentage.denominator + "%")
        Some(candidate)
      } else {
        None
      }
    }
  }

  override def winners(
      election: Election[Ballot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {

    val majorityRational = Rational(1, 2)
    val incrememtSize =
      5 // drop off percentage increases by 5% in each round of elimination till 25%
    var tls = election.firstVotes(ccandidates).toList.sortWith(_._2 > _._2)
    if (tls.size > 2) {
      if (tls.head._2 > majorityRational * election.size) {
        tls.head :: List()
      } else {
        dropOffPercentage = Rational(
          dropOffPercentage.numerator * (100 / dropOffPercentage.denominator) + incrememtSize,
          100
        )
        val losingCand: Option[(Candidate, Rational)] =
          loser(tls.last, election.size, dropOffPercentage)
        losingCand match {
          case Some((c, _)) =>
            val newElection = exclude(election, c)
            winners(newElection, ccandidates.filterNot(x => x == c), numVacancies)
          case None =>
            winners(election, ccandidates, numVacancies)
        }
      }
    } else {
      tls.head :: List()
    }
  }

}
