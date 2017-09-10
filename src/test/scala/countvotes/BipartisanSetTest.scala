package countvotes

import agora.methods.BipartisanSet
import agora.parsers.{CandidatesParser, ParameterParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification

class BipartisanSetTest extends Specification {

  val expectedBipartisanSet = Set(Candidate("A"), Candidate("B"), Candidate("C"))
  val expectedBipartisanSet1 = Set(Candidate("A"), Candidate("B"), Candidate("C"), Candidate("D"), Candidate("E"))

  "BipartisanSet Test " should {

    "verify result" in { bipartisanSetVerification("41-example.e", "37-candidates.txt", "bipartisan-param.json") shouldEqual expectedBipartisanSet }
    "verify result" in { bipartisanSetVerification("38-example.e", "37-candidates.txt", "bipartisan-param1.json") shouldEqual expectedBipartisanSet1 }
  }

  def bipartisanSetVerification(electionFile: String, candidatesFile: String, paramFile: String): Set[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)
    val param = ParameterParser.parse("../Agora/files/Examples/" + paramFile)

    BipartisanSet.bipartisanSet(election, candidates, param).map {_._1}.toSet
  }


}
