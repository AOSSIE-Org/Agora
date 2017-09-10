package agora

import scala.collection.mutable.{HashMap => MMap, HashSet => MSet}
import scala.language.implicitConversions

package object structures {

  abstract sealed class Actions
  case object Exclusion extends Actions
  case object SurplusDistribution extends Actions
  case object ExactWinner extends Actions
  case object Input extends Actions
  case object VictoryWithoutQuota extends Actions
  case object TwoLastCandidatesForOneVacancy extends Actions


}
