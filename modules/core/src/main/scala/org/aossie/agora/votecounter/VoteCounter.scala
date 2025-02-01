package org.aossie.agora.votecounter

import org.aossie.agora.model._
import spire.math.Rational
import org.aossie.agora.votecounter.stv.Input

import scala.language.higherKinds

trait VoteCounter[B[C <: Candidate] <: Ballot[C]] {

  def winners[C <: Candidate](
      election: Election[C, B],
      candidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)]

  def runVoteCounter[C <: Candidate](
      election: Election[C, B],
      candidates: List[C],
      numVacancies: Int
  ): Report[C, B] = {

    val result: Result[C]    = new Result
    val report: Report[C, B] = new Report[C, B]

    var tls = election.firstVotes(candidates)
    result.addTotalsToHistory(tls)
    report.newCount(Input, None, None, Some(tls), None, None)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

}

trait VoteCounterWithCandidate[C <: Candidate, B[CC >: C <: Candidate] <: Ballot[CC]] {

  def winners(
      election: Election[C, B],
      candidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)]

  def runVoteCounter(
      election: Election[C, B],
      candidates: List[C],
      numVacancies: Int
  ): Report[C, B] = {

    val result: Result[C]    = new Result
    val report: Report[C, B] = new Report[C, B]

    var tls = election.firstVotes(candidates)
    result.addTotalsToHistory(tls)
    report.newCount(Input, None, None, Some(tls), None, None)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

}
