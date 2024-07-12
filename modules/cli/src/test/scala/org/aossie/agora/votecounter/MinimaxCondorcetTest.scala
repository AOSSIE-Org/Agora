package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by deepeshpandey on 21/03/17. */
class MinimaxCondorcetTest extends Specification {

  val expectedKemenyYoungWinnerList = List(new Candidate("Nashville"))

  "MinimaxCondorcet Test " should {

    "verify result" in {
      minimaxCondorcetMethodVerification(
        "14-example.e",
        "14-candidates.txt"
      ) shouldEqual expectedKemenyYoungWinnerList
    }
  }

  def minimaxCondorcetMethodVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    MinimaxCondorcet.winners(election, candidates, 1).map(_._1)
  }

}
