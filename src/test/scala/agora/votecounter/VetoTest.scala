package votecounter

import agora.model.Candidate
import agora.parser.CandidatesParser
import agora.parser.PreferencesParser
import agora.votecounter.Veto
import org.specs2.mutable.Specification

class VetoTest extends Specification {

  // Test data as provided by http://democratix.dbai.tuwien.ac.at/examples/veto.php
  val expectedVetoRuleWinnerList = List(Candidate("Banana"), Candidate("Cherry"))

  "VetoRule Test " should {

    "verify result" in {
      vetoRuleMethodVerification(
        "49-example.e",
        "49-candidates.txt"
      ) shouldEqual expectedVetoRuleWinnerList
    }

  }

  def vetoRuleMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Veto.winners(election, candidates, 2).map(_._1)
  }

}
