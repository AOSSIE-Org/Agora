package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

import spire.math.Rational

class SequentialProportionalApprovalVotingTest extends Specification {

  val expectedSequentialProportionalApprovalWinnerList = List(
    (new Candidate("D"), Rational(3, 1)),
    (new Candidate("B"), Rational(4, 1)),
    (new Candidate("A"), Rational(8, 1))
  )

  "SequentialProportionalApprovalVoting Test " should {

    "verify result" in {
      SequentialProportionalApprovalVotingVerification(
        "36-example.e",
        "36-candidates.txt"
      ) shouldEqual expectedSequentialProportionalApprovalWinnerList
    }
  }

  def SequentialProportionalApprovalVotingVerification(
      electionFile: String,
      candidatesFile: String
  ): List[(Candidate, Rational)] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    SequentialProportionalApprovalVoting.winners(election, candidates, 3)
  }

}
