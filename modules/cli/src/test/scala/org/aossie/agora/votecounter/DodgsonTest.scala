package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Example : Page 2
  * https://www.maa.org/sites/default/files/pdf/cmj_ftp/CMJ/September%202010/3%20Articles/6%2009-229%20Ratliff/Dodgson_CMJ_Final.pdf
  */
class DodgsonTest extends Specification {

  val expectedDodgsonWinnerList = List(new Candidate("B"))

  "Dodgson Rule Test " should {

    "verify result" in {
      dodgsonMethodVerification(
        "39-example.e",
        "30-candidates.txt"
      ) shouldEqual expectedDodgsonWinnerList
    }
  }

  def dodgsonMethodVerification(electionFile: String, candidateFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidateFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    Dodgson.winners(election, candidates, 1).map(_._1)
  }

}
