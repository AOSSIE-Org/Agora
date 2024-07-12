package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by deepeshpandey on 27/06/17. */
class CopelandTest extends Specification {

  val expectedCopelandWinnerList  = List(new Candidate("Nashville"))
  val expectedCopelandWinnerList2 = List(new Candidate("A"))

  "Copeland Test " should {

    "verify result" in {
      copelandMethodVerification(
        "14-example.e",
        "14-candidates.txt"
      ) shouldEqual expectedCopelandWinnerList
    }
    "verify result" in {
      copelandMethodVerification(
        "29-example.e",
        "28-candidates.txt"
      ) shouldEqual expectedCopelandWinnerList2
    }
  }

  def copelandMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Copeland.winners(election, candidates, 1).map(_._1)
  }

}
