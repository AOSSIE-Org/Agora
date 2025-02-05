package org.aossie.agora.votecounter;

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class SatisfactionApprovalVotingTest extends Specification {

  val expectedSatisfactionApprovalWinnerList = List(new Candidate("D"), new Candidate("C"))

  "SatisfactionApprovalVoting Test " should {

    "verify result" in {
      SatisfactionApprovalVotingVerification(
        "34-example.e",
        "34-candidates.txt"
      ) shouldEqual expectedSatisfactionApprovalWinnerList
    }
  }

  def SatisfactionApprovalVotingVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    SatisfactionApprovalVoting.winners(election, candidates, 2).map(_._1)
  }

}
