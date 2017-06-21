package countvotes.methods

import countvotes.structures.{Candidate, Input, Rational, Report, _}
import countvotes.structures.{Candidate, Rational, _}
import countvotes.methods.ApprovalRule.countApprovals

import collection.mutable.{HashMap => MMap}
import collection.immutable.{Map => IMap}

/***
  * https://en.wikipedia.org/wiki/Sequential_proportional_approval_voting
  */


object SequentialProportionalApprovalVoting extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    var tls = countApprovals(election, candidates)

    result.addTotalsToHistory(tls)

    report.setCandidates(candidates)
    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def excludeWinner(election: Election[WeightedBallot], winner: List[(Candidate, Rational)]): Election[WeightedBallot] = {
    var list: Election[WeightedBallot]  = Nil
    for(b<-election if !b.preferences.isEmpty){
      if(b.preferences.contains(winner.head._1)){
        list = WeightedBallot(b.preferences.filter(_ !=winner.head._1),b.id,Rational(b.weight.numerator,b.weight.denominator+1)) :: list
      } else {
        list = WeightedBallot(b.preferences,b.id, b.weight) :: list
      }
    }
    list
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    val tls = countApprovals(election, ccandidates)
    val winner = tls.toList.sortWith(_._2 > _._2).take(1).toList
    result.addPendingWinners(winner, None)
    if(numVacancies>1) {
      val newElection = excludeWinner(election, winner)
      winners(newElection, ccandidates.filter(_ != winner.head._1), numVacancies-1)
    } else {
      val results = result.getPendingWinners
      var resultList: List[(Candidate, Rational)] = Nil
      for(b<-results){
        resultList = (b._1,b._2) :: resultList
      }
      resultList
    }
  }
}
