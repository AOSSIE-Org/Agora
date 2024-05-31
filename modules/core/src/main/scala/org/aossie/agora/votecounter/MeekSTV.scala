package org.aossie.agora.votecounter

/** http://blog.opavote.com/2017/04/meek-stv-explained.html
  * https://en.wikipedia.org/wiki/Counting_single_transferable_votes#Meek
  */

import org.aossie.agora.model._
import org.aossie.agora.votecounter.stv._
import collection.mutable.{HashMap => MMap}
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational
import org.aossie.agora.votecounter.stv.Input

object MeekSTV
    extends STV[Ballot]
    with DroopQuota        // Imp
    with NoFractionInQuota // Imp
    with NewWinnersNotOrdered[Ballot]
    with SimpleSurplusDistributionTieResolution // not necessary because of NewWinnersNotOrdered
    with SimpleExclusion
    with UnfairExclusionTieResolution
    with TransferValueWithDenominatorEqualToTotal
    with VoteCounterWithAllBallotsInSurplusDistribution
    with ExactWinnerRemoval {

  val result: Result = new Result

  val report: Report[Ballot] = new Report[Ballot]

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def runVoteCounter(
      election: Election[Ballot],
      candidates: List[Candidate],
      numVacancies: Int
  ): Report[Ballot] = {

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

  def totalsMeek(
      election: Election[Ballot],
      ccandidates: List[Candidate],
      keepFactor: MMap[Candidate, Rational]
  ): MMap[Candidate, Rational] = {
    val scoreMap = new MMap[Candidate, Rational]

    for (b <- election if !b.preferences.isEmpty) {
      var multiplier = Rational(1, 1)
      for (c <- b.preferences) {
        scoreMap(c) = scoreMap.getOrElse(c, Rational(0, 1)) + b.weight * multiplier * keepFactor(c)
        multiplier = multiplier * (Rational(1, 1) - keepFactor(c))
      }
    }
    scoreMap
  }

  def surplusCandidates(totals: MMap[Candidate, Rational], quota: Rational): Int = {
    val surplusCandidatesNumber = totals.filter(x => x._2 >= quota).size
    surplusCandidatesNumber
  }

  def surplusQuantity(totals: MMap[Candidate, Rational], quota: Rational): Rational = {
    val surplusAmount: Rational =
      (Rational(0, 1) /: (totals.filter(_._2 >= quota).map(_._2))) { (surplus, t) =>
        surplus + t - quota
      }
    surplusAmount
  }

  def winnersList(
      election: Election[Ballot],
      ccandidates: List[Candidate],
      numVacancies: Int,
      keepFactor: MMap[Candidate, Rational]
  ): List[(Candidate, Rational)] = {

    println(" \n NEW RECURSIVE CALL \n")

    val tls = totalsMeek(election, ccandidates, keepFactor)
    if (ccandidates.length <= numVacancies) {
      for (c <- ccandidates) yield (c, tls(c))
    } else {
      if (surplusCandidates(tls, result.getQuota) >= numVacancies) {
        tls.toList.sortWith(_._2 > _._2).take(numVacancies)
      } else {
        // Find surplus and check if surplus + last candidate's number of votes < quota
        // If so, then KV = 0 for them
        // Else find surplus ones and reduce their KV
        val surplusAmount = surplusQuantity(tls, result.getQuota)
        val sortedScoreList =
          tls.toList.filter(x => ccandidates.contains(x._1)).sortWith(_._2 < _._2)
        if (
          sortedScoreList.head._2 + surplusAmount < tls.toList
            .filter(x => ccandidates.contains(x._1))
            .sortWith(_._2 < _._2)
            .tail
            .head
            ._2
        ) {
          keepFactor(sortedScoreList.head._1) = Rational(0, 1)
          winnersList(
            exclude(election, sortedScoreList.head._1, None, None)._1,
            ccandidates.filterNot(_ == sortedScoreList.head._1),
            numVacancies,
            keepFactor
          )
        } else {
          val winnerList =
            tls.filter(x => ccandidates.contains(x._1)).filter(_._2 > result.getQuota)
          for (w <- winnerList) {
            keepFactor(w._1) = keepFactor(w._1) * Rational(
              w._2.denominator.toInt * result.getQuota.numerator.toInt,
              w._2.numerator.toInt
            )
          }
          winnersList(election, ccandidates, numVacancies, keepFactor)
        }
      }
    }
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(
      election: Election[Ballot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {
    val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
    // println("Quota = " + quota)
    result.setQuota(quota)

    val keepFactor = new MMap[Candidate, Rational]
    for (c <- ccandidates)
      keepFactor(c) = Rational(1, 1)

    winnersList(election, ccandidates, numVacancies, keepFactor)
  }

}
