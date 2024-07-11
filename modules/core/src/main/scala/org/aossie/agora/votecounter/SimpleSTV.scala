package org.aossie.agora.votecounter

import org.aossie.agora.votecounter.stv._
import org.aossie.agora.model._

import spire.math.Rational
import org.aossie.agora.votecounter.stv.Input

class SimpleSTV[C <: Candidate]
    extends STV[C, PreferenceBallot]
    with DroopQuota
    with NoFractionInQuota
    with NewWinnersNotOrdered[C, PreferenceBallot]
    with SimpleSurplusDistributionTieResolution[C] // not necessary because of NewWinnersNotOrdered
    with SimpleExclusion[C]
    with UnfairExclusionTieResolution[C]
    with TransferValueWithDenominatorEqualToTotal[C]
    with VoteCounterWithAllBallotsInSurplusDistribution[C]
    with ExactWinnerRemoval[C] {

  val result: Result[C] = new Result[C]

  val report: Report[C, PreferenceBallot] = new Report[C, PreferenceBallot]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def runVoteCounter(
      election: Election[C, PreferenceBallot],
      candidates: List[C],
      numVacancies: Int
  ): Report[C, PreferenceBallot] = {
    val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
    println("Quota = " + quota)
    result.setQuota(quota)

    print("\n INPUT ELECTION: \n")
    // printElection(election)

    val tls = election.firstVotes(
      candidates
    ) // Here are totals of candidates also not OCCURING in the ballots
    result.addTotalsToHistory(tls)

    // report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates) // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    println(" \n NEW RECURSIVE CALL \n")

    def mentionedCandidates[B[C <: Candidate] <: PreferenceBallot[C]](
        election: Election[C, B]
    ): List[C] = {
      val set = new collection.mutable.HashSet[C]()
      for (b <- election)
        for (c <- b.preferences)
          if (!set.exists(n => n == c)) set += c
      set.toList
    }

    val ccands = mentionedCandidates(election)

    val tls = election.firstVotes(ccandidates)

    println("Totals: " + tls)

    if (ccands.length <= numVacancies) {
      for (c <- ccands) yield (c, tls(c))
    } else {
      quotaReached(tls, result.getQuota) match {
        case true =>
          println("The quota is reached.")
          val ws: List[(C, Rational)] = returnNewWinners(tls, result.getQuota)
          println("New winners: " + ws)
          result.addPendingWinners(ws.toList, None)

          val vacanciesFilled = ws.length >= numVacancies

          vacanciesFilled match {
            case false =>
              println("Vacancies are not yet filled.")
              val newElection = surplusesDistribution(election, numVacancies - ws.length)
              // printElection(newElection)
              winners(
                newElection,
                ccandidates.filterNot(ws.contains(_)),
                numVacancies - ws.length
              ) ::: ws
            // TODO: care should be taken that newElection is not empty?!
            case true => ws
          }
        case false =>
          val leastVotedCandidate = chooseCandidateForExclusion(tls)
          println("Excluding " + leastVotedCandidate)
          result.addExcludedCandidate(leastVotedCandidate._1, leastVotedCandidate._2)
          val newElection = exclusion(election, leastVotedCandidate._1, numVacancies)
          // printElection(newElection)
          winners(
            newElection,
            ccandidates.filterNot(x => x == leastVotedCandidate._1),
            numVacancies
          )
      }
    }
  }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def surplusesDistribution(
      election: Election[C, PreferenceBallot],
      numVacancies: Int
  ): Election[C, PreferenceBallot] = {
    println("Distribution of surpluses.")
    var newElection = election
    while (result.getPendingWinners.nonEmpty) {
      val (cand, ctotal, markings) = result.takeAndRemoveFirstPendingWinner
      newElection = tryToDistributeSurplusVotes(newElection, cand, ctotal)
    }
    newElection
  }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def tryToDistributeSurplusVotes(
      election: Election[C, PreferenceBallot],
      winner: C,
      ctotal: Rational
  ): Election[C, PreferenceBallot] = {

    val pendingWinners = result.getPendingWinners.map(x => x._1)

    if (ctotal == result.getQuota || !ballotsAreContinuing(winner, election, pendingWinners)) {
      removeWinnerWithoutSurplusFromElection(election, winner)
    } else {
      println("Distributing the surplus of " + winner)
      val surplus = ctotal - result.getQuota

      val tv = computeTransferValue(surplus, election, pendingWinners, winner, None)
      println("tv = " + tv)
      val res = distributeSurplusVotes(election, winner, ctotal, None, pendingWinners, tv)
      res._1
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def exclusion(
      election: Election[C, PreferenceBallot],
      candidate: C,
      numVacancies: Int
  ): Election[C, PreferenceBallot] = {
    println("Exclusion of " + candidate)
    val ex = exclude(election, candidate, None, None)
    ex._1
  }

  override def exclude(
      election: Election[C, PreferenceBallot],
      candidate: C,
      value: Option[Rational],
      newWinners: Option[List[C]]
  ): (Election[C, PreferenceBallot], Set[PreferenceBallot[C]]) = ???

}
