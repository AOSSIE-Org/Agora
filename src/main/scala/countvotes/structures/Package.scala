// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package countvotes


import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import collection.mutable.HashSet
import scala.util.Sorting

import scala.language.implicitConversions



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
  case object TwoLastCandidatesForOneVacancy extends Actions


}
