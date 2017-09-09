package countvotes

import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}
import scala.language.implicitConversions

package object structures {

  type Election[B <: Ballot] = List[B]

  object Election {
    
   def mentionedCandidates[B <: Ballot](election: Election[B]): List[Candidate] = {
     val set = new MSet[Candidate]()
     for (b <- election) {
       for (c <- b.preferences)
         if (!set.exists(n => n == c) ) set += c
      }
     set.toList
    }
    

    lazy val totalWeightedVoters = (election: Election[Ballot]) => {
      election filter { _.preferences.nonEmpty} map {_.weight} reduce { _ + _ }
    }

    implicit def weightedElectionToACTElection(we: Election[Ballot]): Election[ACTBallot] = {
      for (b <- we) yield ACTBallot.fromBallot(b) // b // ACTBallot.fromBallot(b)
    }

    // FIXME: remove this implicit. Rank ballots cannot always be converted to Preferential Ballots.
    implicit def rankedElectionToWeightedElection(re: Election[RankBallot]): Election[Ballot] = {
      for (rb <- re) yield new Ballot(rb.ranks.map(_._1), rb.id, rb.weight)
    }

  }

  abstract sealed class Actions
  case object Exclusion extends Actions
  case object SurplusDistribution extends Actions
  case object ExactWinner extends Actions
  case object Input extends Actions
  case object VictoryWithoutQuota extends Actions
  case object TwoLastCandidatesForOneVacancy extends Actions


}
