package countvotes.methods

import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}

import collection.mutable.{HashMap => MMap}
import collection.immutable.{Map => IMap}

/***
  * https://en.wikipedia.org/wiki/Proportional_approval_voting
  */


object ProportionalApprovalVoting extends VoteCountingMethod[WeightedBallot] {

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

  // following function calculates score, i.e., given N, it calculates summation 1 to 1/N
  def proportionalApprovalScore(nmatches: Int): Rational = {
    var score = Rational(0,1)
    for(i <- 1 to nmatches){
      score = score + Rational(1,i)
    }
    score
  }

  // following function calculates totals for each candidate subsets in the follwoing manner
  // if N of the candidate preferences matches with any one candidate subset,
  // then score for that subset is summation 1 to 1/N
  def candidateSubsetTotals(election: Election[WeightedBallot], candidates: List[Candidate], ccandMap: IMap[Int, List[Candidate]]): List[(Candidate, Rational)] = {
    var scoredCandidateSubsetMap = new MMap[Int, Rational]
    for (a <- ccandMap) {
      for (b <- election if !b.preferences.isEmpty) {
        scoredCandidateSubsetMap(a._1) = scoredCandidateSubsetMap.getOrElse(a._1,Rational(0,1)) + proportionalApprovalScore(b.preferences.length - b.preferences.toSet[Candidate].diff(a._2.toSet[Candidate]).size)
      }
    }
    val sortedCandidateSubsetList = scoredCandidateSubsetMap.toList.sortWith(_._2>_._2)
    val winnerList = ccandMap(sortedCandidateSubsetList.head._1)
    val winnerScore = sortedCandidateSubsetList.head._2
    var finalList: List[(Candidate,Rational)] = Nil
    for(w<-winnerList){
      finalList = (w,winnerScore) :: finalList
    }
    finalList
  }

  // following function creates an immutable map from integers to candidate subsets
  // with each subset size equal to the vacancies available
  def candidateMap(candidates: List[Candidate], numVacancies: Int): IMap[Int, List[Candidate]] = {
    val ccandMap: IMap[Int, List[Candidate]] = candidates.toSet[Candidate].subsets.map(_.toList).toList.filter(x => x.length == numVacancies).zipWithIndex.map{ case(a,b) => b -> a}.toMap
    ccandMap
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    val ccandMap = candidateMap(ccandidates, numVacancies)
    candidateSubsetTotals(election, ccandidates,ccandMap)
  }
}
