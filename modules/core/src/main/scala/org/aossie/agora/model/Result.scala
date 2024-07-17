package org.aossie.agora.model

import spire.math.Rational

import scala.collection.Map

class Result[C <: Candidate] {

  private var quota: Option[Rational] = None

  private var excludedCandidates: List[(C, Rational)] = Nil

  private var pendingWinners: List[(C, Rational, Option[Set[Int]])] = Nil

  private var totalsHistory: List[Map[C, Rational]] =
    Nil // required for ACT's ties resolutions

  private var winners: List[(C, Rational)] = Nil

  def clear: Unit = {
    quota = None
    excludedCandidates = Nil
    pendingWinners = Nil
    totalsHistory = Nil
    winners = Nil
  }

  def setQuota(q: Rational): Unit =
    quota = Some(q)

  def getQuota: Rational = {
    quota match {
      case Some(q) => q
      case None    => throw new Exception("quota is not set yet.")
    }
  }

  def addPendingWinners(
      pendWinners: List[(C, Rational)],
      markings: Option[Set[Int]]
  ): Unit = {
    if (pendWinners.nonEmpty) {
      for (w <- pendWinners)
        if (!pendingWinners.contains(w)) {
          // println("Adding yet undistributed winner " + w + " with markings " + markings.toList.sorted)
          addPendingWinner(w._1, w._2, markings)
        }
    }
  }

  def addPendingWinner(candidate: C, total: Rational, markings: Option[Set[Int]]): Unit =
    // markings match {
    //   case Some(mrks) => pendingWinners = pendingWinners :+ (candidate, total, mrks) // !!! is it adding at the end of the list?
    //  case None => pendingWinners = pendingWinners :+ (candidate, total, None) // !!! is it adding at the end of the list?
    // }
    pendingWinners = pendingWinners :+ (candidate, total, markings)

  def getPendingWinners: List[(C, Rational, Option[Set[Int]])] =
    pendingWinners

  def takeAndRemoveFirstPendingWinner: (C, Rational, Option[Set[Int]]) = {
    val h = pendingWinners.head
    pendingWinners = pendingWinners.tail
    h
  }

  def takeButRetainFirstPendingWinner: (C, Rational, Option[Set[Int]]) =
    pendingWinners.head

  def removePendingWinner(c: C): Unit =
    pendingWinners = pendingWinners.filterNot(p => p._1.name == c.name)

  def addExcludedCandidate(candidate: C, total: Rational): Unit =
    excludedCandidates = (candidate, total) :: excludedCandidates
  // continuingCandidates = continuingCandidates diff List(candidate)

  def addTotalsToHistory(totals: Map[C, Rational]): Unit =
    totalsHistory = totals :: totalsHistory

  def getTotalsHistoryClone: List[Map[C, Rational]] =
    totalsHistory

  def setWinners(ws: List[(C, Rational)]): Unit =
    winners = ws

  def getWinners: List[(C, Rational)] =
    winners

}
