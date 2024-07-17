package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class InstantExhaustiveBallotTest extends Specification {

  val expectedInstantExhaustiveBallotWinnerList = List(new Candidate("Knoxville"))

  "InstantExhaustiveBallot Test " should {

    "verify result" in {
      instantExhaustiveBallotVerification(
        "14-example.e",
        "14-candidates.txt"
      ) shouldEqual expectedInstantExhaustiveBallotWinnerList
    }
  }

  def instantExhaustiveBallotVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    InstantExhaustiveBallot.winners(election, candidates, 1).map(_._1)
  }

}
