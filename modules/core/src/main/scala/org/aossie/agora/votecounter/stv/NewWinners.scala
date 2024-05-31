package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.aossie.agora.votecounter._
import collection.Map

import spire.math.Rational

trait NewWinnersOrderedByTotals[B <: Ballot] extends STV[B] with SurplusDistributionTieResolution {

  def returnNewWinners(
      totals: Map[Candidate, Rational],
      quota: Rational
  ): List[(Candidate, Rational)] = {
    val ws = totals.filter(_._2 >= quota)
    // val lws = ws.toSeq.sortWith(_._2 < _._2).toList
    resolveSurpluseDistributionTie(ws)
  }

}

trait NewWinnersNotOrdered[B <: Ballot] extends STV[B] {

  def returnNewWinners(
      totals: Map[Candidate, Rational],
      quota: Rational
  ): List[(Candidate, Rational)] =
    totals.filter(_._2 >= quota).toList

}
