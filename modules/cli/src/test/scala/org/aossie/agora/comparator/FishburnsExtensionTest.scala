package org.aossie.agora.comparator

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.ParameterParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class FishburnsExtensionTest extends Specification {

  val expectedFishburnSet1 = Set(new Candidate("A"), new Candidate("B"))
  val expectedFishburnSet2 = Set(new Candidate("A"), new Candidate("B"), new Candidate("C"))

  "UnconveredSet Test " should {

    "verify result" in {
      fisburnsExtensionVerification(
        "46-example.e",
        "46-candidates.txt",
        "fishburns-param.json"
      ) shouldEqual expectedFishburnSet1
    }
    "verify result" in {
      fisburnsExtensionVerification(
        "46-example.e",
        "46-candidates.txt",
        "fishburns-param1.json"
      ) shouldEqual expectedFishburnSet2
    }
  }

  def fisburnsExtensionVerification(
      electionFile: String,
      candidatesFile: String,
      parameterFile: String
  ): Set[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)
    val parameters = ParameterParser.parse("../Agora/files/Examples/" + parameterFile)

    FishburnsExtension.compare(election, candidates, parameters)
  }

}
