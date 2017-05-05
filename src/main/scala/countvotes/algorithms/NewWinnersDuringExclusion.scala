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



// for ACT newElection is newElectionWithoutFractionInTotals
trait ACTNewWinnersDuringExclusion extends ACT{
  def declareNewWinnersWhileExcluding(candidate: Candidate,
                                      exhaustedBallots: Set[ACTBallot],
                                      newtotals: Map[Candidate, Rational],
                                      totalsWithoutNewWinners: Map[Candidate, Rational],
                                      newElection: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var newws: List[(Candidate,Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota) ) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, Some(extractMarkings(newElection)))
      //------------ Reporting ------------------------------------------
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(newtotals), Some(newws), Some(exhaustedBallots))
    }
    //------------ Reporting ------------------------------------------
    else {
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    }
    newws
  }
}

// Like ACT, but no markings
trait SenateNewWinnersDuringExclusion extends STV[ACTBallot]{
  def declareNewWinnersWhileExcluding(candidate: Candidate,
                                      exhaustedBallots: Set[ACTBallot],
                                      newtotals: Map[Candidate, Rational],
                                      totalsWithoutNewWinners: Map[Candidate, Rational],
                                      newElection: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var newws: List[(Candidate,Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota)) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, None)
      //------------ Reporting ------------------------------------------
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(newtotals), Some(newws), Some(exhaustedBallots))
    }
    //------------ Reporting ------------------------------------------
    else {
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    }
    newws
  }
}


trait NoNewWinnersDuringExclusion extends ACT{
  def declareNewWinnersWhileExcluding(candidate: Candidate,
                                      exhaustedBallots: Set[ACTBallot],
                                      newtotals: Map[Candidate, Rational],
                                      totalsWithoutNewWinners: Map[Candidate, Rational],
                                      newElectionWithoutFractionInTotals: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    report.newCount(Exclusion, Some(candidate), Some(newElectionWithoutFractionInTotals), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    Nil
  }
}
