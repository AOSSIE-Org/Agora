package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class BaldwinTest extends Specification {

  val expectedBaldwinWinnerList = List(Candidate("A"))

  "Baldwin Test " should {

    "verify result" in { baldwinVerification("13-example.e") shouldEqual expectedBaldwinWinnerList }
  }

  def baldwinVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/13-candidates.txt")
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    BaldwinMethod.winners(election, candidates, candidates.length).map(_._1)
  }

}
