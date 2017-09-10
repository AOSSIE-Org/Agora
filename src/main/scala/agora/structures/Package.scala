package agora

import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}
import scala.language.implicitConversions

package object structures {

  abstract sealed class Action
  case object Exclusion extends Action
  case object SurplusDistribution extends Action
  case object ExactWinner extends Action
  case object Input extends Action
  case object VictoryWithoutQuota extends Action
  case object TwoLastCandidatesForOneVacancy extends Action


}
