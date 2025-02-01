package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math._

abstract class Egalitarian[C <: Candidate] extends VoteCounterWithCandidate[C, PreferenceBallot] {

  val fairness: Double

  def rank(b: PreferenceBallot[C], c: C, numCandidates: Int): Int = {
    val r = b.preferences.indexOf(c)
    if (r != -1) r else numCandidates
  }

  def utilityIndividual(b: PreferenceBallot[C], c: C, numCandidates: Int): Int =
    numCandidates - rank(b, c, numCandidates)

  def utilitySet(b: PreferenceBallot[C], cs: Iterable[C]): Int =
    cs.map(c => utilityIndividual(b, c, cs.size)).reduce(_ + _)

  def socialWelfare(e: Election[C, PreferenceBallot], cs: Iterable[C]): Rational = {
    e.ballots.foldLeft(Rational(0, 1)) { (acc, b) =>
      acc + (b.weight) * pow(utilitySet(b, cs), 1 / fairness)
    }
  }

}
