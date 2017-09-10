package agora.votecounter

import agora.votecounter.KemenyYoung
import agora.parser.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification


/**
  * This test is to verify the election results with the wikipedia example https://en.wikipedia.org/wiki/Kemeny–Young_method
  */
class KemenyYoungTest extends Specification{

  val expectedKemenyYoungWinnerList = List(Candidate("Nashville"), Candidate("Chattanooga"), Candidate("Knoxville"), Candidate("Memphis"))

  "KemenyYoung Test " should {

    "verify result" in { kemenyYoungMethodVerification("14-example.e") shouldEqual expectedKemenyYoungWinnerList }
  }

  def kemenyYoungMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    KemenyYoung.winners(election, candidates, candidates.length).map {_._1}
  }
}
