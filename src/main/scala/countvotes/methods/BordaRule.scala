package countvotes.methods

import countvotes.structures.{Candidate, Rational, _}
import collection.mutable.{HashMap => Map}

/**
  * Created by deepeshpandey on 07/03/17.
  */
abstract class BordaRule[B <: WeightedBallot with Weight] extends VoteCountingMethod[B] {
  protected val result: Result = new Result
  protected val report: Report[B] = new Report[B]

  override def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]

    for (c<-candidates) m(c) = 0

    for (b <- election if !b.preferences.isEmpty) {
      // need to take the size of the list first and then calculate the borda scores
      var preferencesLength = b.preferences.length

      for (candidate <- b.preferences) {
        m(candidate) = m(candidate) + (preferencesLength-1)
        preferencesLength -= 1
      }

    }
    m
  }
}
