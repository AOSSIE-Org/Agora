package countvotes

import countvotes.methods.{KemenyYoungMethod, MinimaxCondorcetMethod}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

/**
  * Created by deepeshpandey on 21/03/17.
  */
class MinimaxCondorcetTest extends Specification{

  val expectedKemenyYoungWinnerList = List(Candidate("Nashville"))

  "MinimaxCondorcet Test " should {

    "verify result" in { kemenyYoungMethodVerification("14-example.txt") shouldEqual expectedKemenyYoungWinnerList }
  }

  def kemenyYoungMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    MinimaxCondorcetMethod.winners(election, candidates, 1).map {_._1}
  }

}
