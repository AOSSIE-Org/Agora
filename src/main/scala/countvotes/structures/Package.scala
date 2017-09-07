package countvotes

import scala.collection.mutable.{HashMap => Map}
import scala.language.implicitConversions

package object structures {

  type Election[B <: Ballot] = List[B]

  object Election {

    lazy val totalWeightedVoters = (election: Election[WeightedBallot]) => {
      election filter { _.preferences.nonEmpty} map {_.weight} reduce { _ + _ }
    }

    implicit def weightedElectionToACTElection(we: Election[WeightedBallot]): Election[ACTBallot] = {
      for (b <- we) yield ACTBallot.fromWeightedBallot(b) // b // ACTBallot.fromWeightedBallot(b)
    }

    implicit def rankedElectionToWeightedElection(re: Election[RankedWeightedBallot]): Election[WeightedBallot] = {
      for (b <- re) yield RankedWeightedBallot.toWeightedBallot(b)
    }

    implicit def scoredElectionToWeightedElection(re: Election[ScoredWeightedBallot]): Election[WeightedBallot] = {
      for (b <- re) yield ScoredWeightedBallot.toWeightedBallot(b)
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
