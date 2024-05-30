package votecounter

import agora.model.Candidate
import agora.parser.CandidatesParser
import agora.parser.PreferencesParser
import agora.votecounter.Bucklin
import org.specs2.mutable.Specification

class BucklinTest extends Specification {

  // Test data obtained from https://en.wikipedia.org/wiki/Bucklin_voting#Example_application
  val expectedBucklinWinnerList = List(Candidate("Nashville"))

  "Bucklin Vote Test " should {

    "verify result" in {
      bucklinVerification("48-example.e", "48-candidates.txt") shouldEqual expectedBucklinWinnerList
    }
  }

  def bucklinVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Bucklin.winners(election, candidates, 1).map(_._1)
  }

}
