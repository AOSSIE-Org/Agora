package agora.votecounter

import agora.parser.CandidatesParser
import agora.parser.PreferencesParser
import agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by lebedka on 12/5/17. */
class MajorityRuleTest extends Specification {

  val expectedMajorityRuleWinnerList1 = List()
  val expectedMajorityRuleWinnerList2 = List(Candidate("A"))

  "MajorityRule Test " should {

    "verify result" in {
      majorityRuleMethodVerification(
        "01-example.e",
        "01-candidates.txt"
      ) shouldEqual expectedMajorityRuleWinnerList1
    }
    "verify result" in {
      majorityRuleMethodVerification(
        "02-example.e",
        "02-candidates.txt"
      ) shouldEqual expectedMajorityRuleWinnerList2
    }

  }

  def majorityRuleMethodVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Majority.winners(election, candidates, candidates.length).map(_._1)
  }

}
