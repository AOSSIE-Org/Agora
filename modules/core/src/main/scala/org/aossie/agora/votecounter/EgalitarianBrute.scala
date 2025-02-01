package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational

import scala.language.postfixOps

class EgalitarianBrute[C <: Candidate](val fairness: Double = 2) extends Egalitarian[C] {

  def winners(
      e: Election[C, Ballot],
      cs: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    val candidateSubsets    = cs.toSet.subsets(numVacancies)
    val bestCandidateSubset = candidateSubsets.maxBy(socialWelfare(e, _))
    bestCandidateSubset.map((_, Rational(1, 1))) toList
  }

}
