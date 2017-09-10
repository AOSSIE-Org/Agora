package agora.methods.stv

import agora.structures._
import agora.methods._
import collection.mutable.{HashMap => Map}
import scala.util.Random

import spire.math.Rational

trait ACTTotalsDuringExclusion extends ACT{


  def computeIncorrectTotalofEVACS(step: (Candidate, Rational), newElectionWithoutFractionInTotals: Election[ACTBallot]): Option[Int] = {
    val roundedExcludedTotal = computeRoundedExcludedTotal(step, newElectionWithoutFractionInTotals)
    val previousTotalOfTheCandidate = result.getTotalsHistoryClone.head(step._1).toInt  // TODO: take care here, check that it is correct
    val newTotal = previousTotalOfTheCandidate - roundedExcludedTotal
    println("ACT's total of candidate being excluded: " + newTotal)
    Some(newTotal)
  }

  def computeRoundedExcludedTotal(step: (Candidate, Rational), election: Election[ACTBallot]): Int =
  {
    var numOccurences = 0
    for (b <- election)
      if (b.preferences.head == step._1 && b.value == step._2) {
        numOccurences += 1
      }
    val total = numOccurences * step._2
    val roundedtotal = total.toBigDecimal(0, java.math.RoundingMode.DOWN).toInt
    roundedtotal
  }

  def rewriteTotalOfCandidate(totals: Map[Candidate, Rational], candidate: Candidate, newTotal: Option[Int]): Map[Candidate, Rational] = {
    var newmap = totals
    newTotal match {
      case Some(t) => newmap(candidate) = t
      case None =>
    }
    newmap
  }

}



// Totals as the sum of weights of ballots in partial exclusion (in contrast to how it is done in EVACS)
trait RegularTotalsDuringExclusion {

  def rewriteTotalOfCandidate(totals: Map[Candidate, Rational], candidate: Candidate, newTotal: Option[Int]): Map[Candidate, Rational] = {
    totals
  }

  def computeIncorrectTotalofEVACS(step: (Candidate, Rational), newElectionWithoutFractionInTotals: Election[ACTBallot]): Option[Int] = None

}
