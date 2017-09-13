package agora.votecounter

import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational

import scala.language.postfixOps

class EgalitarianBrute(val fairness: Double = 2) extends Egalitarian[Ballot] {

  def winners(e: Election[Ballot], cs: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {
    val candidateSubsets = cs.toSet.subsets(numVacancies)
    val bestCandidateSubset = candidateSubsets.maxBy(socialWelfare(e, _))
    bestCandidateSubset map { (_, Rational(1,1)) } toList
  }
}
