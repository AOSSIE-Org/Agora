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

  def proportionalApprovalScore(nmatches: Int): Rational = {
    var score = Rational(0,1)
    for(i <- 1 to nmatches){
      score = score + Rational(1,i)
    }
    score
  }

  def totals1(election: Election[WeightedBallot], candidates: List[Candidate], ccandMap: IMap[Int, List[Candidate]]): List[(Candidate, Rational)] = {
    var m = new MMap[Int, Rational]
    for (a <- ccandMap) {
      for (b <- election if !b.preferences.isEmpty) {
        m(a._1) = m.getOrElse(a._1,Rational(0,1)) + proportionalApprovalScore(b.preferences.length - b.preferences.toSet[Candidate].diff(a._2.toSet[Candidate]).size)
      }
    }
    val winnerList = ccandMap(m.toList.sortWith(_._2>_._2).head._1)
    val winnerScore = m.toList.sortWith(_._2>_._2).head._2
    var finalList: List[(Candidate,Rational)] = Nil
    for(w<-winnerList){
      finalList = (w,winnerScore) :: finalList
    }
    finalList
  }

  def candidateMap(candidates: List[Candidate], numVacancies: Int): IMap[Int, List[Candidate]] = {
    val ccandMap: IMap[Int, List[Candidate]] = candidates.toSet[Candidate].subsets.map(_.toList).toList.filter(x => x.length == numVacancies).zipWithIndex.map{ case(a,b) => b -> a}.toMap
    ccandMap
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    val ccandMap = candidateMap(ccandidates, numVacancies)
    totals1(election, ccandidates,ccandMap)
  }
}
