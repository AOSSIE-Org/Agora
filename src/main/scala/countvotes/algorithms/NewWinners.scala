package countvotes.algorithms

import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}
import countvotes.methods._
import collection.mutable.{HashMap => Map}



trait NewWinnersOrderedByTotals[B <: Ballot] extends STV[B] with SurplusDistributionTieResolution{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    val ws = totals.clone().retain((k,v) => v >= quota)
    // val lws = ws.toSeq.sortWith(_._2 < _._2).toList
    resolveSurpluseDistributionTie(ws)
  }
}





trait NewWinnersNotOrdered[B <: Ballot] extends STV[B]{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    totals.clone().retain((k,v) => v >= quota).toList
  }
}
