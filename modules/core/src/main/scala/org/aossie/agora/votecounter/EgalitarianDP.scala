package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import spire.math.Rational

import scala.collection.mutable.{HashMap => MMap}

class EgalitarianDP(val fairness: Double = 2) extends Egalitarian[Ballot] {
  val idealCandidates = new MMap[(Int,Set[Candidate]), List[Candidate]] ()

  def winners(e: Election[Ballot], cs: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {
    val winningCandidates: List[Candidate] = recursiveWinnersComputation(e, cs, numVacancies)
    winningCandidates map { (_, Rational(1,1)) }
  }

  def recursiveWinnersComputation(e: Election[Ballot], cs: List[Candidate], numVacancies: Int): List[Candidate] = numVacancies match {
    case 0 => List.empty
    case n => {
      if(idealCandidates.contains((numVacancies,cs.toSet))) {idealCandidates((numVacancies,cs.toSet))}
      val candidateSets : List[List[Candidate]] = cs.map(x => recursiveWinnersComputation(e, cs.filterNot(elem => elem == x), numVacancies-1) :+ x)
      val result = candidateSets.maxBy(socialWelfare(e, _))
      idealCandidates += (((numVacancies,cs.toSet),result))
      result
    }
  }
}
