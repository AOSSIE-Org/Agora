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

package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import java.io._


trait ACTExactWinnerRemoval extends STV[ACTBallot]{


  def removeWinnerWithoutSurplusFromElection(election: Election[ACTBallot], winner: Candidate): Election[ACTBallot] = {
   var list: Election[ACTBallot] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =  ACTBallot(filterPreferences(b.preferences, winner::List()),  b.id, b.marking, b.weight, b.value)::list
      }
   list
  }

}

// exactly like ACTExactWinnerRemoval
trait SenateExactWinnerRemoval extends STV[ACTBallot]{

  def removeWinnerWithoutSurplusFromElection(election: Election[ACTBallot], winner: Candidate): Election[ACTBallot] = {
   var list: Election[ACTBallot] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =  ACTBallot(filterPreferences(b.preferences, winner::List()),  b.id, b.marking, b.weight, b.value)::list
      }
   list
  }

}



trait ExactWinnerRemoval extends STV[WeightedBallot]{


  def removeWinnerWithoutSurplusFromElection(election: Election[WeightedBallot], winner: Candidate): Election[WeightedBallot] = {
   var list: Election[WeightedBallot] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =  WeightedBallot(filterPreferences(b.preferences, winner::List()),  b.id,  b.weight)::list
      }
   list
  }

}
