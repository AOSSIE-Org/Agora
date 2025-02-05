package org.aossie.agora.votecounter

import org.aossie.agora.model.Candidate
import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.specs2.mutable.Specification

class SimpleSTVRuleTest extends Specification {

  // Winners in list in order of merit as expected from election results at https://en.wikipedia.org/wiki/Single_transferable_vote
  val expectedSimpleRuleWinnerList =
    List(new Candidate("Chocolate"), new Candidate("Oranges"), new Candidate("Strawberries"))

  "SimpleSTVRule Test " should {
    // Election data taken from https://en.wikipedia.org/wiki/Single_transferable_vote
    "verify result" in {
      simpleRuleMethodVerification(
        "47-example.e",
        "47-candidates.txt"
      ) shouldEqual expectedSimpleRuleWinnerList.reverse
    }
  }

  def simpleRuleMethodVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    (new SimpleSTV).runVoteCounter(election, candidates, 3).getWinners.map(_._1)
  }

}
