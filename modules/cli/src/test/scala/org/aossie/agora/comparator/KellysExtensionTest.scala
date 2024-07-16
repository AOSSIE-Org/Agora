package org.aossie.agora.comparator

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.ParameterParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class KellysExtensionTest extends Specification {

  val expectedKellySet1 = Set(new Candidate("A"))
  val expectedKellySet2 = Set(new Candidate("A"), new Candidate("B"))
  val expectedKellySet3 = Set(new Candidate("B"))
  val expectedKellySet4 = Set(new Candidate("B"), new Candidate("C"))

  "UnconveredSet Test " should {

    "verify result" in {
      kellysExtensionVerification(
        "45-example.e",
        "45-candidates.txt",
        "kellys-sets.json"
      ) shouldEqual expectedKellySet1
    }
    "verify result" in {
      kellysExtensionVerification(
        "45-example.e",
        "45-candidates.txt",
        "kellys-sets1.json"
      ) shouldEqual expectedKellySet2
    }
    "verify result" in {
      kellysExtensionVerification(
        "45-example.e",
        "45-candidates.txt",
        "kellys-sets2.json"
      ) shouldEqual expectedKellySet3
    }
    "verify result" in {
      kellysExtensionVerification(
        "45-example.e",
        "45-candidates.txt",
        "kellys-sets3.json"
      ) shouldEqual expectedKellySet4
    }
    "verify result" in {
      kellysExtensionVerification(
        "45-example.e",
        "45-candidates.txt",
        "kellys-sets4.json"
      ) shouldEqual expectedKellySet2
    }
  }

  def kellysExtensionVerification(
      electionFile: String,
      candidatesFile: String,
      parameterFile: String
  ): Set[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)
    val parameters = ParameterParser.parse("./files/Examples/" + parameterFile)

    KellyExtension.compare(election, candidates, parameters)
  }

}
