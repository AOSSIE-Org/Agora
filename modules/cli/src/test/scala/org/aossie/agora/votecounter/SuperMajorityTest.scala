package org.aossie.agora.votecounter

import org.aossie.agora.model.Candidate
import org.aossie.agora.model.Parameters
import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.ParameterParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.votecounter.SuperMajority
import org.specs2.mutable.Specification
import spire.math.Rational

class SuperMajorityTest extends Specification {

  // From the file provided it can be seen that candidate A wins with 60% of the votes
  val expectedSuperMajorityRuleWinnerList1 = List(Candidate("A"))
  val expectedSuperMajorityRuleWinnerList2 = List()

  "SuperMajorityRule Test " should {

    "verify result" in {
      superMajorityRuleMethodVerification(
        "02-example.e",
        "02-candidates.txt",
        "supermajority-param.json"
      ) shouldEqual expectedSuperMajorityRuleWinnerList1
    }

    "verify result" in {
      superMajorityRuleMethodVerification(
        "02-example.e",
        "02-candidates.txt",
        "supermajority-param2.json"
      ) shouldEqual expectedSuperMajorityRuleWinnerList2
    }
  }

  def superMajorityRuleMethodVerification(
      electionFile: String,
      candidatesFile: String,
      parametersFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)
    val parameters = ParameterParser.parse("../Agora/files/Examples/" + parametersFile)
    SuperMajority
      .runVoteCounter(election, candidates, candidates.length, parameters)
      .getWinners
      .map(_._1)
  }

}
