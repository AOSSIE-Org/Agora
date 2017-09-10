package agora.methods

import agora.methods.BaldwinMethod.bordaScores
import agora.structures._
import agora.structures.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => Map}

import spire.math.Rational

/**
  * https://en.wikipedia.org/wiki/Nanson%27s_method
  */
object Nanson extends VoteCounter[Ballot] {

  def winners(election: Election[Ballot], candidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {

    if (candidates.length == 1) {
      bordaScores(election, candidates).toList
    } else {
      var cbs = bordaScores(election, candidates) //borda scores of candidates
      val average = (Rational(0,1) /: candidates)(_ + cbs(_)) / Rational(candidates.length)
      winners(election, candidates.filter(x => cbs(x) > average), numVacancies)
    }
  }
}
