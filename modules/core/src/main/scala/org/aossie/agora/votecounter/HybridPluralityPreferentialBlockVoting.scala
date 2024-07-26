package org.aossie.agora.votecounter

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Preferential_block_voting */

object HybridPluralityPreferentialBlockVoting extends VoteCounter[PreferenceBallot] {

  def totalsForFirstNVacancies[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): MMap[C, Rational] = {
    // calculates the totals for first n candidates where n is equal to number of vacancies
    var m = new MMap[C, Rational]
    for (b <- election if !b.preferences.isEmpty)
      for (c <- b.preferences.take(numVacancies))
        m(c) = m.getOrElse(c, Rational(0, 1)) + b.weight
    m
  }

  def exclude[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidate: C
  ): Election[C, PreferenceBallot] = {
    var list: List[PreferenceBallot[C]] = Nil
    for (b <- election if !b.preferences.isEmpty)
      list = new PreferenceBallot(b.preferences.filter(_ != ccandidate), b.id, b.weight) :: list
    Election(list)
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    var winnerlist: List[(C, Rational)] = Nil
    var election1                       = election
    var ccandidates1                    = ccandidates
    var vacancies                       = numVacancies
    while (vacancies != 0) {
      var tls            = totalsForFirstNVacancies(election1, ccandidates1, vacancies)
      val sortedCandList = tls.toList.sortWith(_._2 > _._2)
      if (sortedCandList.head._2 > (election1.size / 2)) {
        winnerlist = sortedCandList.head :: winnerlist
        election1 = exclude(election1, sortedCandList.head._1)
        ccandidates1 = ccandidates1.filter(_ != sortedCandList.head._1)
        vacancies = vacancies - 1
      } else {
        election1 = exclude(
          election1,
          tls.filter(x => ccandidates1.contains(x._1)).toList.sortWith(_._2 < _._2).head._1
        )
        ccandidates1 = ccandidates1.filter(_ != sortedCandList.last._1)
      }
    }
    winnerlist
  }

}
