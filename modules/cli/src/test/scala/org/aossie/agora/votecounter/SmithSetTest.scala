package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by deepeshpandey on 16/07/17. */
class SmithSetTest extends Specification {

  val expectedSmithSet = Set(new Candidate("A"), new Candidate("B"), new Candidate("C"))

  "SmithSet Test " should {

    "verify result" in {
      topCycleSetVerification("30-example.e", "30-candidates.txt") shouldEqual expectedSmithSet
    }
  }

  def topCycleSetVerification(electionFile: String, candidatesFile: String): Set[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    SmithSet.winners(election, candidates, 1).map(_._1).toSet
  }

}
