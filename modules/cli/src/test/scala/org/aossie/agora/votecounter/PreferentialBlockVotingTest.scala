package org.aossie.agora.votecounter;

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class PreferentialBlockVotingTest extends Specification {

  val expectedPreferentialBlockVotingWinnerList = List(new Candidate("Sue"))

  "PreferentialBlockVoting Test " should {

    "verify result" in {
      PreferentialBlockVotingVerification(
        "32-example.e",
        "32-candidates.txt"
      ) shouldEqual expectedPreferentialBlockVotingWinnerList
    }
  }

  def PreferentialBlockVotingVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    PreferentialBlockVoting.winners(election, candidates, 1).map(_._1)
  }

}
