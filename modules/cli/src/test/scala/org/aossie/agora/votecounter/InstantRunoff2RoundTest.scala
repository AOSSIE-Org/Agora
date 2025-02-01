package org.aossie.agora.votecounter;

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by deepeshpandey on 03/06/17. */
class InstantRunoff2RoundTest extends Specification {

  val expectedRunoff2RoundMethodWinnerList = List(new Candidate("icecream"))

  "Runoff2Round Test " should {

    "verify result" in {
      runoff2RoundMethodVerification(
        "17-example.e",
        "17-candidates.txt"
      ) shouldEqual expectedRunoff2RoundMethodWinnerList
    }
  }

  def runoff2RoundMethodVerification(
      electionFile: String,
      candidatesFile: String
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    InstantRunoff2Round.winners(election, candidates, 1).map(_._1)
  }

}
