package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class CoombTest extends Specification {

  val expectedCoombWinnerList = List(new Candidate("Nashville"))
  val expectedCoombWinnerList1 = List(
    new Candidate("B")
  ) // tie resolution test case both A/B could be a winner here

  "Coomb Test" should {
    "verify result" in {
      coombMethodVerification(
        "14-example.e",
        "14-candidates.txt"
      ) shouldEqual expectedCoombWinnerList
      coombMethodVerification(
        "23-example.e",
        "23-candidates.txt"
      ) shouldEqual expectedCoombWinnerList1
    }
  }

  def coombMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    Coomb.winners(election, candidates, 1).map(_._1)

  }

}
