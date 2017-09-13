package agora.votecounter

import agora.model._
import agora.model.PreferenceBallot

import spire.math._

abstract class Egalitarian[B <: PreferenceBallot] extends VoteCounter[B] {
  val fairness: Double

  def rank(b: PreferenceBallot, c: Candidate): Option[Int] = {
    val r = b.preferences.indexOf(c)
    r match {
      case -1 => None
      case _ => Some(r)
    }
  }

  def utilityIndividual(b: PreferenceBallot, c: Candidate, numCandidates: Int): Int = {
    rank(b,c) match {  
      case Some(rank) => numCandidates - rank
      case _ => 0
    }
  }

  def utilitySet(b: PreferenceBallot, cs: Traversable[Candidate]): Int = {
    cs map { c => utilityIndividual(b, c, cs.size)} reduce { _ + _ }
  }

  def socialWelfare(e: Election[B], cs: Traversable[Candidate]): Rational = {
    (Rational(0,1) /: e.ballots) { (acc, b) => acc + (b.weight) * exp((1/fairness) * log(utilitySet(b,cs))) } 
  }
}
