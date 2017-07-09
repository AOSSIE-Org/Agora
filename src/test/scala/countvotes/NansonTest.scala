package countvotes

import countvotes.methods.NansonMethod
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification


class NansonTest extends Specification{

  val expectedNansonWinnerList = List(Candidate("A"))

  "Nanson Test " should {

    "verify result" in { nansonVerification("13-example.txt") shouldEqual expectedNansonWinnerList }
  }

  def nansonVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/13-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    NansonMethod.winners(election, candidates, candidates.length).map {_._1}
  }
}
