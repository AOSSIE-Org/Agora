package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}

import spire.math.Rational

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
  val result: Result
  
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
