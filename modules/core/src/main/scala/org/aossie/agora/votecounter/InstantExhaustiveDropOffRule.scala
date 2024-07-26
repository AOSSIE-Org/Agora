package org.aossie.agora.votecounter

import org.aossie.agora.votecounter.stv._
import org.aossie.agora.model._

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Exhaustive_ballot#Notes */

object InstantExhaustiveDropOffRule
    extends VoteCounter[PreferenceBallot]
    with SimpleExclusionWithFixedElectionSize {

  var dropOffPercentage = Rational(0, 100)

  val cutoffPercentage =
    25 // drop off of candidates in steps of 5% till 25% after which lowest scoring candidate is eliminated

  def loser[C <: Candidate](
      candidate: (C, Rational),
      total: Int,
      dropOffPercentage: Rational
  ): Option[(C, Rational)] = {
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

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

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
        val losingCand: Option[(C, Rational)] =
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
