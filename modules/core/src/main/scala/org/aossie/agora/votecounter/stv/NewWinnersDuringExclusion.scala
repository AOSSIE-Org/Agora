package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.votecounter._

import collection.Map

import spire.math.Rational

// for ACT newElection is newElectionWithoutFractionInTotals
trait ACTNewWinnersDuringExclusion[C <: Candidate] extends ACT[C] {

  def declareNewWinnersWhileExcluding(
      candidate: C,
      exhaustedBallots: Set[ACTBallot[C]],
      newtotals: Map[C, Rational],
      totalsWithoutNewWinners: Map[C, Rational],
      newElection: Election[C, ACTBallot]
  ): List[(C, Rational)] = {
    var newws: List[(C, Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota)) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, Some(extractMarkings(newElection)))
      // ------------ Reporting ------------------------------------------
      report.newCount(
        Exclusion,
        Some(candidate),
        Some(newElection),
        Some(newtotals),
        Some(newws),
        Some(exhaustedBallots)
      )
    }
    // ------------ Reporting ------------------------------------------
    else {
      report.newCount(
        Exclusion,
        Some(candidate),
        Some(newElection),
        Some(totalsWithoutNewWinners),
        None,
        Some(exhaustedBallots)
      )
    }
    newws
  }

}

// Like ACT, but no markings
trait SenateNewWinnersDuringExclusion[C <: Candidate] extends STV[C, ACTBallot] {

  val result: Result[C]

  val report: Report[C, ACTBallot]

  def declareNewWinnersWhileExcluding(
      candidate: C,
      exhaustedBallots: Set[ACTBallot[C]],
      newtotals: Map[C, Rational],
      totalsWithoutNewWinners: Map[C, Rational],
      newElection: Election[C, ACTBallot]
  ): List[(C, Rational)] = {
    var newws: List[(C, Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota)) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, None)
      // ------------ Reporting ------------------------------------------
      report.newCount(
        Exclusion,
        Some(candidate),
        Some(newElection),
        Some(newtotals),
        Some(newws),
        Some(exhaustedBallots)
      )
    }
    // ------------ Reporting ------------------------------------------
    else {
      report.newCount(
        Exclusion,
        Some(candidate),
        Some(newElection),
        Some(totalsWithoutNewWinners),
        None,
        Some(exhaustedBallots)
      )
    }
    newws
  }

}

trait NoNewWinnersDuringExclusion[C <: Candidate] extends ACT[C] {

  def declareNewWinnersWhileExcluding(
      candidate: C,
      exhaustedBallots: Set[ACTBallot[C]],
      newtotals: Map[C, Rational],
      totalsWithoutNewWinners: Map[C, Rational],
      newElectionWithoutFractionInTotals: Election[C, ACTBallot]
  ): List[(C, Rational)] = {
    report.newCount(
      Exclusion,
      Some(candidate),
      Some(newElectionWithoutFractionInTotals),
      Some(totalsWithoutNewWinners),
      None,
      Some(exhaustedBallots)
    )
    Nil
  }

}
