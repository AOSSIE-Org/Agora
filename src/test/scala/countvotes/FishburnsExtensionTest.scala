package countvotes

import countvotes.methods.FishburnsExtensionMethod
import countvotes.parsers.{CandidatesParser, ParameterParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

class FishburnsExtensionTest extends Specification {

  val expectedFishburnSet1 = Set(Candidate("A"), Candidate("B"))
  val expectedFishburnSet2 = Set(Candidate("A"), Candidate("B"), Candidate("C"))

  "UnconveredSet Test " should {

    "verify result" in { fisburnsExtensionVerification("42-example.e", "42-candidates.txt", "fishburns-param.json") shouldEqual expectedFishburnSet1 }
    "verify result" in { fisburnsExtensionVerification("42-example.e", "42-candidates.txt", "fishburns-param1.json") shouldEqual expectedFishburnSet2 }
  }

  def fisburnsExtensionVerification(electionFile: String, candidatesFile: String, parameterFile: String): Set[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)
    val parameters = ParameterParser.parse("../Agora/files/Examples/" + parameterFile)

    FishburnsExtensionMethod.fishburnPreferredSet(election, candidates, parameters).map {_._1}.toSet
  }


}
