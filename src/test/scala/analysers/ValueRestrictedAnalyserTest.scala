package analysers

import countvotes.analysers.ValueRestrictedAnalyser
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import org.specs2.mutable.Specification


class ValueRestrictedAnalyserTest extends Specification {

  val expectedAnalysis1 = true
  val expectedAnalysis2 = false

  "Value Restricted Analyser " should {

    "verify result" in { valueRestrictedAnalyserVerification("22-example.e", "13-candidates.txt") shouldEqual expectedAnalysis1 }
    "verify result" in { valueRestrictedAnalyserVerification("31-example.e", "13-candidates.txt") shouldEqual expectedAnalysis2 }
  }

  def valueRestrictedAnalyserVerification(electionFile: String, candidateFile: String): Boolean = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidateFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    ValueRestrictedAnalyser.analyse(election, candidates)
  }

}
