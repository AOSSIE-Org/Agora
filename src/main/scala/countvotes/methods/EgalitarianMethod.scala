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
import scala.math._

object EgalitarianMethod extends Egalitarian[WeightedBallot] {
  var allCandidates: List[Candidate] = List.empty

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):  Report[WeightedBallot] = {
    allCandidates = candidates
    println("Number of WeightedBallots: " + election.length)
    report.setWinners(winners(election, allCandidates, numVacancies))
    report
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {
    val candidateCount: Int = allCandidates.length

    val candidateSubsets: List[List[Candidate]] = getCandidateSubsets(ccandidates,candidateCount,List.empty,0,numVacancies)

    if(candidateCount < numVacancies) {println("not enough candidates") }

    var currentMax: Double = 0
    var currentCandidates: List[Candidate] = List.empty

    for(i <- candidateSubsets){
      var contemplatedMax: Double = 0
      contemplatedMax = socialWelfare(election, i)
      if(currentMax < contemplatedMax){
        currentMax = contemplatedMax
        currentCandidates = i
      }
    }
    var currentCandidatesForReturn : List[(Candidate,Rational)] = List.empty
    for(i <- currentCandidates){
      currentCandidatesForReturn = currentCandidatesForReturn :+ (i,new Rational(n=1,d=1))
    }
    currentCandidatesForReturn
  }

  def getCandidateSubsets(candidates: List[Candidate], candidateCount: Int, stub: List[Candidate], location: Int, remaining: Int): List[List[Candidate]] = {
    var combinations: List[List[Candidate]] = List.empty
    if(remaining == 1){
      for(i <- location to (candidateCount-1)){ //Kept in "old" system as different subsets are used, this seems the clearest way to do it
        var completedSubset: List[Candidate] = stub :+ candidates(i)
        combinations = combinations :+ completedSubset
      }
    }
    else{
      for(i <- location to (candidateCount-remaining)) {
        combinations = combinations ::: getCandidateSubsets(candidates, candidateCount, stub :+ candidates(i),i + 1, remaining - 1)
      }
    }
    combinations
  }
}
