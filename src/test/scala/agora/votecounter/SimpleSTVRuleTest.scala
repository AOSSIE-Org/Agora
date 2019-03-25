package votecounter

import agora.model.Candidate
import agora.parser.{CandidatesParser, PreferencesParser}
import agora.votecounter.SimpleSTV
import org.specs2.mutable.Specification

class SimpleSTVRuleTest extends Specification {

  //Winners in list in order of merit as expected from election results at https://en.wikipedia.org/wiki/Single_transferable_vote
  val expectedSimpleRuleWinnerList = List(Candidate("Chocolate"), Candidate("Oranges"), Candidate("Strawberries"))

  "SimpleSTVRule Test " should {
    //Election data taken from https://en.wikipedia.org/wiki/Single_transferable_vote
    "verify result" in { simpleRuleMethodVerification("47-example.e", "47-candidates.txt") shouldEqual expectedSimpleRuleWinnerList.reverse }
  }

  def simpleRuleMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    (new SimpleSTV).runVoteCounter(election, candidates, 3).getWinners.map {_._1}
  }
}
