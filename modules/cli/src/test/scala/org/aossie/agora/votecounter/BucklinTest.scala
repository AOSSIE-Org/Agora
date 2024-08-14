package org.aossie.agora.votecounter

import org.aossie.agora.model.Candidate
import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.specs2.mutable.Specification

class BucklinTest extends Specification {

  // Test data obtained from https://en.wikipedia.org/wiki/Bucklin_voting#Example_application
  val expectedBucklinWinnerList = List(new Candidate("Nashville"))

  "Bucklin Vote Test " should {

    "verify result" in {
      bucklinVerification("48-example.e", "48-candidates.txt") shouldEqual expectedBucklinWinnerList
    }
  }

  def bucklinVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    Bucklin.winners(election, candidates, 1).map(_._1)
  }

}
