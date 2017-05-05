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


trait ACTNewWinnersDuringSurplusesDistribution extends ACT{
  def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var ws:  List[(Candidate,Rational)] = List()
    if (quotaReached(totals, result.getQuota)){
     ws = returnNewWinners(totals, result.getQuota) // sorted for further surplus distribution!
     result.addPendingWinners(ws.toList, Some(extractMarkings(election)))
    }
    ws
  }
}


// Like ACTNewWinnersDuringSurplusesDistribution, but None instead of markings
trait SenateNewWinnersDuringSurplusesDistribution extends STV[ACTBallot]{
  def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var ws:  List[(Candidate,Rational)] = List()
    if (quotaReached(totals, result.getQuota)){
     ws = returnNewWinners(totals, result.getQuota) // sorted for further surplus distribution!
     result.addPendingWinners(ws.toList, None)
    }
    ws
  }
}



trait NoNewWinnersDuringSurplusesDistribution{
  def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)] = {
    List()
  }
}
