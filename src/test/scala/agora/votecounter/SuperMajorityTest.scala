package votecounter

import agora.model.{Candidate, Parameters}
import agora.parser.{CandidatesParser, PreferencesParser}
import agora.votecounter.SuperMajority
import org.specs2.mutable.Specification
import spire.math.Rational

class SuperMajorityTest extends Specification{

  //From the file provided it can be seen that candidate A wins with 60% of the votes
  val expectedSuperMajorityRuleWinnerList1 = List(Candidate("A"))
  val expectedSuperMajorityRuleWinnerList2 = List()
  val goodPercentage : Double = Rational(55, 100).doubleValue()
  val badPercentage : Double = Rational(61, 100).doubleValue()


  "SuperMajorityRule Test " should {

    "verify result" in { superMajorityRuleMethodVerification(
      "02-example.e",
      "02-candidates.txt",
      Parameters(majorityPercentage = Some(goodPercentage))) shouldEqual expectedSuperMajorityRuleWinnerList1 }

    "verify result" in { superMajorityRuleMethodVerification(
      "02-example.e",
      "02-candidates.txt",
      Parameters(majorityPercentage = Some(badPercentage))) shouldEqual expectedSuperMajorityRuleWinnerList2 }
  }

  def superMajorityRuleMethodVerification(electionFile: String, candidatesFile: String, parameters: Parameters): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)
    SuperMajority.runVoteCounter(election, candidates, candidates.length, parameters).getWinners.map {_._1}
  }
}
