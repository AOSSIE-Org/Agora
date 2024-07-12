package org.aossie.agora.votecounter

import org.aossie.agora.model.Candidate
import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.votecounter.Veto
import org.specs2.mutable.Specification

class VetoTest extends Specification {

  // Test data as provided by http://democratix.dbai.tuwien.ac.at/examples/veto.php
  val expectedVetoRuleWinnerList = List(new Candidate("Banana"), new Candidate("Cherry"))

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
