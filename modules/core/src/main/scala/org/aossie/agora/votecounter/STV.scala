package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import scala.language.higherKinds

//import scala.collection.mutable.{HashMap => MMap}

import scala.collection.Map

import spire.math.Rational

abstract class STV[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]]
    extends VoteCounter[C, B] {

  def computeQuota(numVotes: Int, numVacancies: Int): Rational

  def cutQuotaFraction(num: Rational): Rational

  def returnNewWinners(
      totals: Map[C, Rational],
      quota: Rational
  ): List[(C, Rational)]

  def computeTransferValue(
      surplus: Rational,
      election: Election[C, B],
      pendingWinners: List[C],
      candidate: C,
      markings: Option[Set[Int]]
  ): Rational

  def distributeSurplusVotes(
      election: Election[C, B],
      candidate: C,
      total: Rational,
      markings: Option[Set[Int]],
      pendingWinners: List[C],
      transferValue: Rational
  ): (Election[C, B], Set[B[C]], Option[Election[C, B]])

  def resolveSurpluseDistributionTie(
      equaltotals: Map[C, Rational]
  ): List[(C, Rational)]

  // def run(e: Election[B], numVacancies: Int):   Report[B] = {
  //  val output = runVoteCounter(e: Election[B], numVacancies: Int)
  //  result.clear
  //  report.clear
  //  output
  // }

  def sumTotals(totals: Map[C, Rational]): Rational = {
    var sum: Rational = 0
    for (t <- totals)
      sum += t._2
    sum
  }

  def computeTotal(election: Election[C, B], candidate: C): Rational = {
    var r: Rational = 0
    for (b <- election if !b.preferences.isEmpty && b.preferences.head == candidate)
      r = r + b.weight
    r
  }

  def quotaReached(totals: Map[C, Rational], quota: Rational): Boolean = {
    if (totals.exists(_._2 >= quota)) {
      println("\nQuota: reached")
      true
    } else {
      println("\nQuota: not reached")
      false
    }
  }

  def ballotsAreContinuing(
      c: C,
      election: Election[C, B],
      pendingWinners: List[C]
  ): Boolean = {
    var el       = election
    var ballotsC = false
    while (ballotsC == false && el.nonEmpty) {
      val ballot = el.head
      if (ballot.preferences.head == c && !ballot.preferences.tail.diff(pendingWinners).isEmpty) {
        ballotsC = true
      }
      el = el.tail
    }
    println("Has continuing candidates?: " + ballotsC)
    ballotsC
  }

  // TODO: Optimize: as soon as we found continuing candidate, we can simply attach the rest of the list
  def filterPreferences(
      preferences: List[C],
      candidates: List[C]
  ): List[C] = {
    var newpreferences: List[C] = Nil
    for (c <- preferences) {
      candidates.exists(x => x == c) match {
        case true  =>
        case false => newpreferences = c :: newpreferences
      }
    }
    newpreferences.reverse
  }

}
