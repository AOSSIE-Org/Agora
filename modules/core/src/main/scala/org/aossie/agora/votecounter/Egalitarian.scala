package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.PreferenceBallot

import spire.math._

abstract class Egalitarian[B <: PreferenceBallot] extends VoteCounter[B] {
  val fairness: Double

  def rank(b: PreferenceBallot, c: Candidate, numCandidates: Int): Int = {
    val r = b.preferences.indexOf(c)
    if (r != -1) r else numCandidates
  }

  def utilityIndividual(b: PreferenceBallot, c: Candidate, numCandidates: Int): Int = {
    numCandidates - rank(b, c, numCandidates)
  }

  def utilitySet(b: PreferenceBallot, cs: Traversable[Candidate]): Int = {
    cs map { c => utilityIndividual(b, c, cs.size)} reduce { _ + _ }
  }

  def socialWelfare(e: Election[B], cs: Traversable[Candidate]): Rational = {
    (Rational(0,1) /: e.ballots) { (acc, b) => acc + (b.weight) * pow(utilitySet(b,cs), (1/fairness)) } 
  }
}
