import countvotes.methods.MeekSTV
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate,Rational}
import org.specs2.mutable.Specification


class MeekSTVTest extends Specification {

  val expectedMeekSTVWinnerList = List(Candidate("C"), Candidate("A"), Candidate("B"))

  "MeekSTV Test " should {

    "verify result" in { meekSTVVerification("41-example.e", "41-candidates.txt") shouldEqual expectedMeekSTVWinnerList}
  }

  def meekSTVVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    MeekSTV.winners(election, candidates, 3).map {_._1}
  }
}
