package countvotes


import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import collection.mutable.HashSet
import scala.util.Sorting

import scala.languageFeature.implicitConversions

package object structures {
  
  type Election[B <: Ballot] = List[B]
  
  object Election {
    implicit def weightedElectionToACTElection(we: Election[WeightedBallot]): Election[ACTBallot] = {
      for (b <- we) yield ACTBallot.fromWeightedBallot(b) // b // ACTBallot.fromWeightedBallot(b)
    } 
  }
       
  abstract sealed class Actions
  case object Exclusion extends Actions
  case object SurplusDistribution extends Actions
  case object ExactWinner extends Actions
  case object Input extends Actions
  case object VictoryWithoutQuota extends Actions
  
  
}