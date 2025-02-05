package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

import spire.math.Rational

class ProportionalApprovalVotingTest extends Specification {

  val expectedProportionalApprovalWinnerList =
    List((new Candidate("A"), Rational(61, 2)), (new Candidate("C"), Rational(61, 2)))

  "ProportionalApprovalVoting Test " should {

    "verify result" in {
      proportionalApprovalVotingVerification(
        "35-example.e",
        "35-candidates.txt"
      ) shouldEqual expectedProportionalApprovalWinnerList
    }
  }

  def proportionalApprovalVotingVerification(
      electionFile: String,
      candidatesFile: String
  ): List[(Candidate, Rational)] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    ProportionalApprovalVoting.winners(election, candidates, 2)
  }

}
