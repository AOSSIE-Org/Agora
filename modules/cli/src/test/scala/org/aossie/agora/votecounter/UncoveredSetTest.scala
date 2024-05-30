package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Test class for unconvered set for the preference profile in 21-example.e */
class UncoveredSetTest extends Specification {

  val expectedUncoveredSet = Set(Candidate("A"), Candidate("B"), Candidate("C"))

  "UnconveredSet Test " should {

    "verify result" in {
      unconveredSetMethodVerification(
        "30-example.e",
        "29-candidates.txt"
      ) shouldEqual expectedUncoveredSet
    }
  }

  def unconveredSetMethodVerification(
      electionFile: String,
      candidatesFile: String
  ): Set[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    UncoveredSet.winners(election, candidates, candidates.length).map(_._1).toSet
  }

}
