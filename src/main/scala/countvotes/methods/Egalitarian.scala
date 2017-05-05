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

abstract class Egalitarian[B <: WeightedBallot with Weight] extends VoteCountingMethod[B] {
  val report: Report[B] = new Report[B]
  val fairness: Double = 2
  var allCandidates: List[Candidate];

  /*
  def getCandidateList(election: Election[B]): List[Candidate] = {
    var candidateList:List[Candidate] = List()
    for(i <- election){
      for(j <- i.preferences){
        if(!(candidateList contains j)){
          candidateList = candidateList :+ j
        }
      }
    }
    candidateList
  }
  */


  def rank(election: Election[B], voter: Int, candidate: Candidate): (Boolean,Int) = {
    for(i <- 0 to (election(voter).preferences.length-1)) {
      if(election(voter).preferences(i) == candidate){
        (true,i)
      }
    }
    (false,0)
  }

  def utilityIndividual(election: Election[B], voter: Int, candidate: Candidate): Int = rank(election,voter,candidate) match {
    case (true,rank) => allCandidates.length - rank
    case _ => 0
  }

  def utilitySet(election: Election[B], voter: Int, candidates: List[Candidate]): Int = {
    val sum: Int = candidates map { c => utilityIndividual(election, voter, c)} reduce { _ + _ }
    sum
  }

  def socialWelfare(election: Election[B], candidates: List[Candidate]): Double = {
    var sum: Double = 0
    for(i <- 0 to (election.length-1)){
      sum = sum + (election(i).weight).toDouble * exp((1/fairness) * log(utilitySet(election,i,candidates)))
    }
    sum
  }

  def maxTuple1(x:(Double, List[Candidate]), y:(Double, List[Candidate])): (Double, List[Candidate]) = {
    if (x._1 > y._1) {x}
    else {y}
  }
}
