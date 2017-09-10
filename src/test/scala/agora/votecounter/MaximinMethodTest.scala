package agora.votecounter

import agora.votecounter.Maximin
import agora.parser.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification


class MaximinMethodTest extends Specification{

  val expectedMaximinMethodWinnerList = List(Candidate("Nashville"))

  "Maximin Method Test " should {

    "verify result" in { maximinMethodVerification("14-example.e", "14-candidates.txt") shouldEqual expectedMaximinMethodWinnerList }
  }

  def maximinMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Maximin.winners(election, candidates, 1).map {_._1}
  }

}
