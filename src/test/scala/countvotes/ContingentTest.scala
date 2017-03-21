package countvotes

import countvotes.methods.ContingentMethod
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification



class ContingentTest extends Specification{

  val expectedContingentWinner = List(Candidate("Nashville"))

  "Contingent Test " should {

    "verify result" in { ContingentMethodVerification("14-example.txt") shouldEqual expectedContingentWinner }
  }

  def ContingentMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    ContingentMethod.winners(election, candidates, 1).map {_._1}
  }
}
