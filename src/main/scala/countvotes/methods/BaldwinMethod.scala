package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 09/03/17.
  */
object BaldwinMethod extends VoteCounter[Ballot] with LazyLogging {

  def bordaScores(election: Election[Ballot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]

    for (b <- election if !b.preferences.isEmpty) {
      // need to take the size of the list first and then calculate the borda scores
      var bordaCounter = candidates.length
      b.preferences.filter(candidate => candidates.contains(candidate)).map(candidate => {
        m(candidate) = m.getOrElse(candidate, new Rational(0, 1)) + ((bordaCounter - 1) * b.weight.numerator.toInt)
        bordaCounter -= 1
      })
    }
    m
  }

  def winners(election: Election[Ballot], candidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {

    logger.info("Baldwin rule method called")

    if (candidates.length == 1) {
      bordaScores(election, candidates).toList
    } else {
        // removing the lowest borda score candidate from the candidate list
        var lowestBordaCandidate = bordaScores(election, candidates).minBy(_._2)
        winners(election, candidates.filter(_ != lowestBordaCandidate._1), numVacancies)
      }
    }
  }
