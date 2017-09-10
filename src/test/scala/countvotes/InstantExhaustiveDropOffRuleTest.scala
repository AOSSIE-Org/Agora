import agora.methods.InstantExhaustiveDropOffRule
import agora.parsers.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification


class InstantExhaustiveDropOffRuleTest extends Specification {

  val expectedInstantExhaustiveDropOffRuleWinnerList = List(Candidate("Knoxville"))

  "InstantExhaustiveDropOffRule Test " should {

    "verify result" in { instantExhaustiveDropOffRuleVerification("14-example.e", "14-candidates.txt") shouldEqual expectedInstantExhaustiveDropOffRuleWinnerList }
  }

  def instantExhaustiveDropOffRuleVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    InstantExhaustiveDropOffRule.winners(election, candidates, 1).map {_._1}
  }
}
