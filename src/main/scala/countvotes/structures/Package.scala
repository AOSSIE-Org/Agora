package countvotes

import scala.collection.mutable.{HashMap => Map}
import scala.language.implicitConversions

package object structures {

  type Election[B <: Ballot] = List[B]

  object Election {

    lazy val totalWeightedVoters = (election: Election[Ballot]) => {
      election filter { _.preferences.nonEmpty} map {_.weight} reduce { _ + _ }
    }

    implicit def weightedElectionToACTElection(we: Election[Ballot]): Election[ACTBallot] = {
      for (b <- we) yield ACTBallot.fromBallot(b) // b // ACTBallot.fromBallot(b)
    }

    implicit def rankedElectionToWeightedElection(re: Election[RankBallot]): Election[Ballot] = {
      for (b <- re) yield RankBallot.toBallot(b)
    }

    implicit def scoredElectionToWeightedElection(re: Election[ScoreBallot]): Election[Ballot] = {
      for (b <- re) yield ScoreBallot.toBallot(b)
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
