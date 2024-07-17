package org.aossie.agora.votecounter

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Preferential_block_voting */

object HybridPluralityPreferentialBlockVoting extends VoteCounter[Candidate, PreferenceBallot] {

  def totalsForFirstNVacancies(
      election: Election[Candidate, PreferenceBallot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): MMap[Candidate, Rational] = {
    // calculates the totals for first n candidates where n is equal to number of vacancies
    var m = new MMap[Candidate, Rational]
    for (b <- election if !b.preferences.isEmpty)
      for (c <- b.preferences.take(numVacancies))
        m(c) = m.getOrElse(c, Rational(0, 1)) + b.weight
    m
  }

  def exclude(
      election: Election[Candidate, PreferenceBallot],
      ccandidate: Candidate
  ): Election[Candidate, PreferenceBallot] = {
    var list: List[PreferenceBallot[Candidate]] = Nil
    for (b <- election if !b.preferences.isEmpty)
      list = new PreferenceBallot(b.preferences.filter(_ != ccandidate), b.id, b.weight) :: list
    Election(list)
  }

  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(
      election: Election[Candidate, PreferenceBallot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {
    var winnerlist: List[(Candidate, Rational)] = Nil
    var election1                               = election
    var ccandidates1                            = ccandidates
    var vacancies                               = numVacancies
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
