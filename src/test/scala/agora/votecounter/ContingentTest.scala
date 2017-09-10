package agora.votecounter

import agora.parser.{CandidatesParser, PreferencesParser}
import agora.model.Candidate
import org.specs2.mutable.Specification

//To verify tests using example from Wikipedia - https://en.wikipedia.org/wiki/Contingent_vote#Example_I

class ContingentTest extends Specification{

  val expectedContingentWinner = List(Candidate("Catherine"))

  "Contingent Test " should {

    "verify result" in { contingentMethodVerification("15-example.e") shouldEqual expectedContingentWinner }
  }

  def contingentMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/15-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Contingent.winners(election, candidates, 1).map {_._1}
  }
}
