package agora.votecounter

import agora.votecounter.{Dodgson}
import agora.parser.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification

/**
  * Example : Page 2 https://www.maa.org/sites/default/files/pdf/cmj_ftp/CMJ/September%202010/3%20Articles/6%2009-229%20Ratliff/Dodgson_CMJ_Final.pdf
  */
class DodgsonTest extends Specification {

  val expectedDodgsonWinnerList = List(Candidate("B"))

  "Dodgson Rule Test " should {

    "verify result" in { dodgsonMethodVerification("39-example.e", "30-candidates.txt") shouldEqual expectedDodgsonWinnerList }
  }

  def dodgsonMethodVerification(electionFile: String, candidateFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidateFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Dodgson.winners(election, candidates, 1) map {_._1}
  }

}
