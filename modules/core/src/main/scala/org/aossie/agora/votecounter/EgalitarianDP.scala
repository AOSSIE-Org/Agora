package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import spire.math.Rational

import scala.collection.mutable.{HashMap => MMap}

class EgalitarianDP[C <: Candidate](val fairness: Double = 2) extends Egalitarian[C] {

  val idealCandidates = new MMap[(Int, Set[C]), List[C]]()

  def winners(
      e: Election[C, Ballot],
      cs: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    val winningCandidates: List[C] = recursiveWinnersComputation(e, cs, numVacancies)
    winningCandidates.map((_, Rational(1, 1)))
  }

  def recursiveWinnersComputation(
      e: Election[C, Ballot],
      cs: List[C],
      numVacancies: Int
  ): List[C] = numVacancies match {
    case 0 => List.empty
    case n =>
      if (idealCandidates.contains((numVacancies, cs.toSet))) {
        idealCandidates((numVacancies, cs.toSet))
      }
      val candidateSets: List[List[C]] = cs.map(x =>
        recursiveWinnersComputation(e, cs.filterNot(elem => elem == x), numVacancies - 1) :+ x
      )
      val result = candidateSets.maxBy(socialWelfare(e, _))
      idealCandidates += (((numVacancies, cs.toSet), result))
      result
  }

}
