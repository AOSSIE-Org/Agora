package countvotes.methods

import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}

import collection.mutable.{HashMap => MMap}
import collection.immutable.{Map => IMap}

/***
  * https://en.wikipedia.org/wiki/Satisfaction_approval_voting
  */


object SatisfactionApprovalVoting extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

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

  // follwoing function takes the integer mapping of candidate subsets and calculates totals
  // if all the candidates chosen by a voter is a subset of the candidate subsets in IMap
  // then score is 1, else score decreases by 1/n for each candidate which is not included in the candidate subsets of IMap
  def totalsForSubsets(election: Election[WeightedBallot], candidates: List[Candidate], ccandMap: IMap[Int, List[Candidate]]): List[(Candidate, Rational)] = {
    var scoredSubsetMap = new MMap[Int, Rational]
    for (a <- ccandMap) {
      for (b <- election if !b.preferences.isEmpty) {
        scoredSubsetMap(a._1) = scoredSubsetMap.getOrElse(a._1,Rational(0,1)) + Rational(b.preferences.length - b.preferences.toSet[Candidate].diff(a._2.toSet[Candidate]).size, b.preferences.length)
      }
    }
    val sortedScoredSubsetMap = scoredSubsetMap.toList.sortWith(_._2>_._2)
    val winnerList = ccandMap(sortedScoredSubsetMap.head._1)
    val winnerScore = sortedScoredSubsetMap.head._2
    var finalList: List[(Candidate,Rational)] = Nil
    for(w<-winnerList){
      finalList = (w,winnerScore) :: finalList
    }
    finalList
  }

  // following function returns immutable map that maps candidate subsets to integers, which are then referred to for calculating totals
  // each candidate subset has length equal to numVacancies
  def candidateMap(candidates: List[Candidate], numVacancies: Int): IMap[Int, List[Candidate]] = {
    val ccandMap: IMap[Int, List[Candidate]] = candidates.toSet[Candidate].subsets.map(_.toList).toList.filter(x => x.length == numVacancies).zipWithIndex.map{ case(a,b) => b -> a}.toMap
    ccandMap
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    // following code makes use of additive satisfcation property of Satisfaction Approval Voting
    val candidateScoreMap = new MMap[Candidate, Rational]
    for(b<-election if !b.preferences.isEmpty){
      for(c<-b.preferences){
        candidateScoreMap(c) = Rational(1, b.preferences.size) + candidateScoreMap.getOrElse(c, 0)
      }
    }
    candidateScoreMap.toList.sortWith(_._2>_._2).take(numVacancies)
    //val ccandMap = candidateMap(ccandidates, numVacancies)
    //totalsForSubsets(election, ccandidates,ccandMap)
  }
}
