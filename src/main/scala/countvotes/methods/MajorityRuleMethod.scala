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

package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

object MajorityRuleMethod extends MajorityRule[WeightedBallot] {

  val majorityThreshold = Rational(1,2)

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {
      print("\n INPUT ELECTION: \n")
      printElection(election)

      var tls = totals(election, candidates)
      result.addTotalsToHistory(tls)

      report.setCandidates(candidates)
      report.newCount(Input, None, Some(election), Some(tls), None, None)

      report.setWinners(winners(election, candidates, numVacancies))

      report
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
      totals(election, ccandidates).toList sortWith {
        (ct1, ct2) => ct1._2 > ct2._2
      } take(numVacancies) filter { case (c, t) => t > majorityThreshold }
  }
}

