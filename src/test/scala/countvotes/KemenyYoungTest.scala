package countvotes

import countvotes.methods.KemenyYoungMethod
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification


/**
  * This test is to verify the election results with the wikipedia example https://en.wikipedia.org/wiki/Kemeny–Young_method
  */
class KemenyYoungTest extends Specification{

  val expectedKemenyYoungWinnerList = List(Candidate("Nashville", None, Some("")), Candidate("Chattanooga", None, Some("")), Candidate("Knoxville", None, Some("")), Candidate("Memphis", None, Some("")))

  "KemenyYoung Test " should {

    "verify result" in { kemenyYoungMethodVerification("14-example.e") shouldEqual expectedKemenyYoungWinnerList }
  }

  def kemenyYoungMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    KemenyYoungMethod.winners(election, candidates, candidates.length).map {_._1}
  }
}
