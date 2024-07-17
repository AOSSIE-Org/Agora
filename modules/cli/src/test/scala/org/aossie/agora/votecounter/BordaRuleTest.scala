package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by deepeshpandey on 19/06/17. */
class BordaRuleTest extends Specification {

  val expectedBordaWinnerList = List(
    new Candidate("Nashville"),
    new Candidate("Chattanooga"),
    new Candidate("Memphis"),
    new Candidate("Knoxville")
  )

  "Borda Rule Test " should {

    "verify result" in {
      bordaRuleMethodVerification("14-example.e") shouldEqual expectedBordaWinnerList
    }
  }

  def bordaRuleMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/14-candidates.txt")
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    Borda.winners(election, candidates, candidates.length).map(_._1)
  }

}
