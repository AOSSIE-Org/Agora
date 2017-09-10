package agora.votecounter.stv

import scala.language.implicitConversions


abstract sealed class Action
case object Exclusion extends Action
case object SurplusDistribution extends Action
case object ExactWinner extends Action
case object Input extends Action
case object VictoryWithoutQuota extends Action
case object TwoLastCandidatesForOneVacancy extends Action


