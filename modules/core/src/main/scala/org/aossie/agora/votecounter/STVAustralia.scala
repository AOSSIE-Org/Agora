package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math.Rational
import org.aossie.agora.votecounter.stv.Input
import org.aossie.agora.votecounter.stv.ACTBallot

abstract class STVAustralia[C <: Candidate] extends STV[C, ACTBallot] {

  val result: Result[C] = new Result[C]()

  val report = new Report[C, ACTBallot]

  def tryToDistributeSurplusVotes(
      election: Election[C, ACTBallot],
      ccandidates: List[C],
      winner: C,
      ctotal: Rational,
      markings: Option[Set[Int]]
  ): (Election[C, ACTBallot], List[(C, Rational)])

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // FIXME: This is an ugly hack that should be removed after we get rid of ACTBallot
  def runVoteCounterGeneral(
      election: Election[C, PreferenceBallot],
      candidates: List[C],
      numVacancies: Int
  ): Report[C, PreferenceBallot] = {

    val r  = runVoteCounter(convertBallots(election), candidates, numVacancies)
    val r1 = new Report[C, PreferenceBallot]
    r1.setWinners(r.getWinners)
    r1
  }

  // FIXME: This is an ugly hack that should be removed after we get rid of ACTBallot
  def convertBallots(we: Election[C, PreferenceBallot]): Election[C, ACTBallot] =
    new Election(for (b <- we) yield ACTBallot.fromBallot(b)) // b // ACTBallot.fromBallot(b)

  override def runVoteCounter(
      election: Election[C, ACTBallot],
      candidates: List[C],
      numVacancies: Int
  ): Report[C, ACTBallot] = { // all ballots of e are marked when the function is called
    val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
    println("Number of ballots:" + election.length)
    println("Quota: " + quota)
    result.setQuota(quota)
    report.setQuota(quota)

    val tls = election.firstVotes(candidates) // Here are totals also of those candidates
    // that are NOT OCCURING in the ballots (i.e. when nobody mentioned them in preferences)
    result.addTotalsToHistory(tls)

    // report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(
      candidates
    ) // Here are totals also of those candidates that are NOT OCCURING in the ballots

    report.newCount(Input, None, Some(election), Some(tls), None, None)
    report.setLossByFractionToZero

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // ACT Legislation:
  // 9(1): If a candidate is excluded in accordance with clause 8, the ballot papers counted for the candidate
  // shall be sorted into groups according to their transfer values when counted for him or her.
  //
  // like ACT
  // Senate Legislation:
  // (13AA)(a) and (13AA)(b)
  //

  def determineStepsOfExclusion(
      election: Election[C, ACTBallot],
      candidate: C
  ): List[(C, Rational)] = {
    var s: Set[(C, Rational)] = Set()

    for (b <- election) {
      if (
        b.preferences.nonEmpty && b.preferences.head == candidate && !s.contains(
          (candidate, b.value)
        )
      ) {
        s += ((candidate, b.value))
      }
    }
    s.toList.sortBy(x => x._2).reverse // >
  }

}
