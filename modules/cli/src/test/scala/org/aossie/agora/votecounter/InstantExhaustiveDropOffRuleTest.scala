package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class InstantExhaustiveDropOffRuleTest extends Specification {

  val expectedInstantExhaustiveDropOffRuleWinnerList = List(new Candidate("Knoxville"))

  "InstantExhaustiveDropOffRule Test " should {

    "verify result" in {
      instantExhaustiveDropOffRuleVerification(
        "14-example.e",
        "14-candidates.txt"
      ) shouldEqual expectedInstantExhaustiveDropOffRuleWinnerList
    }
  }

  def instantExhaustiveDropOffRuleVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    InstantExhaustiveDropOffRule.winners(election, candidates, 1).map(_._1)
  }

}
