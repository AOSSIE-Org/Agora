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

trait NoNewWinnersDuringSurplusesDistribution{
  def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)] = {
    List()
  }
}