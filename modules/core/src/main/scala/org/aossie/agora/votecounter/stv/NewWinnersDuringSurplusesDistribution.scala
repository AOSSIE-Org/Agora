package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.votecounter._

import collection.Map

import spire.math.Rational

trait ACTNewWinnersDuringSurplusesDistribution[C <: Candidate] extends ACT[C] {

  def declareNewWinnersWhileDistributingSurpluses(
      totals: Map[C, Rational],
      election: Election[C, ACTBallot]
  ): List[(C, Rational)] = {
    var ws: List[(C, Rational)] = List()
    if (quotaReached(totals, result.getQuota)) {
      ws = returnNewWinners(totals, result.getQuota) // sorted for further surplus distribution!
      result.addPendingWinners(ws, Some(extractMarkings(election)))
    }
    ws
  }

}

// Like ACTNewWinnersDuringSurplusesDistribution, but None instead of markings
trait SenateNewWinnersDuringSurplusesDistribution[C <: Candidate] extends STV[C, ACTBallot] {

  val result: Result[C]

  def declareNewWinnersWhileDistributingSurpluses(
      totals: Map[C, Rational],
      election: Election[C, ACTBallot]
  ): List[(C, Rational)] = {
    var ws: List[(C, Rational)] = List()
    if (quotaReached(totals, result.getQuota)) {
      ws = returnNewWinners(totals, result.getQuota) // sorted for further surplus distribution!
      result.addPendingWinners(ws.toList, None)
    }
    ws
  }

}

trait NoNewWinnersDuringSurplusesDistribution[C <: Candidate] {

  def declareNewWinnersWhileDistributingSurpluses(
      totals: Map[C, Rational],
      election: Election[C, ACTBallot]
  ): List[(C, Rational)] =
    List()

}
