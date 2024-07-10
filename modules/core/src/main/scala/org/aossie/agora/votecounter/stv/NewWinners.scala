package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.votecounter._

import collection.Map
import spire.math.Rational

trait NewWinnersOrderedByTotals[C <: Candidate, B[CC >: C <: Candidate] <: PreferenceBallot[CC]]
    extends STV[C, B]
    with SurplusDistributionTieResolution {

  def returnNewWinners(
      totals: Map[C, Rational],
      quota: Rational
  ): List[(C, Rational)] = {
    val ws = totals.filter(_._2 >= quota)
    // val lws = ws.toSeq.sortWith(_._2 < _._2).toList
    resolveSurpluseDistributionTie(ws)
  }

}

trait NewWinnersNotOrdered[C <: Candidate, B[CC >: C <: Candidate] <: PreferenceBallot[CC]]
    extends STV[C, B] {

  def returnNewWinners(
      totals: Map[C, Rational],
      quota: Rational
  ): List[(C, Rational)] =
    totals.filter(_._2 >= quota).toList

}
