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
  def candidateSubsetTotals(election: Election[WeightedBallot], candidates: List[Candidate], ccandSubsetList: List[List[Candidate]]): List[(Candidate, Rational)] = {
    var scoredCandidateSubsetMap = new MMap[List[Candidate], Rational]
    for (a<-ccandSubsetList) {
      for (b<-election) {
        scoredCandidateSubsetMap(a) = scoredCandidateSubsetMap.getOrElse(a, Rational(0,1)) + proportionalApprovalScore(b.preferences.length - b.preferences.toSet[Candidate].diff(a.toSet[Candidate]).size)
      }
    }
    val sortedCandidateSubsetList = scoredCandidateSubsetMap.toList.sortWith(_._2 > _._2)
    val winnerList = sortedCandidateSubsetList.head._1
    val winnerScore = sortedCandidateSubsetList.head._2
    val finalList = winnerList map { (_, winnerScore) }
    finalList
  }

  // generates subsets of length k of list of candidates in recursive manner
  def candidateSubsetListGenerator(k: Int, candidates: List[Candidate]) : List[List[Candidate]] = {
    candidates match {
      case Nil => Nil
      case head :: tail =>
        if (k <= 0 || k > candidates.length) {
          Nil
        } else if (k == 1) {
          candidates.map(List(_))
        } else {
          candidateSubsetListGenerator(k - 1, tail).map(head :: _) ::: candidateSubsetListGenerator(k, tail)
        }
    }
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    val ccandSubsetList = candidateSubsetListGenerator(numVacancies, ccandidates)
    candidateSubsetTotals(election, ccandidates,ccandSubsetList)
  }
}
