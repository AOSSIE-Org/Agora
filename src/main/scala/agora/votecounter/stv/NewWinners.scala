package agora.votecounter.stv

import agora.model._
import agora.model.{PreferenceBallot => Ballot}
import agora.votecounter._
import collection.mutable.{HashMap => MMap}
import collection.Map

import spire.math.Rational

trait NewWinnersOrderedByTotals[B <: Ballot] extends STV[B] with SurplusDistributionTieResolution{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    val ws = totals.filter(_._2 >= quota)
    // val lws = ws.toSeq.sortWith(_._2 < _._2).toList
    resolveSurpluseDistributionTie(ws)
  }
}





trait NewWinnersNotOrdered[B <: Ballot] extends STV[B]{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    totals.filter(_._2 >= quota).toList
  }
}
