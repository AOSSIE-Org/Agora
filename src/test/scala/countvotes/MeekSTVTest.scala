import countvotes.methods.MeekSTV
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate,Rational}
import org.specs2.mutable.Specification


class MeekSTVTest extends Specification {

  val expectedMeekSTVWinnerList = List(Candidate("C"), Candidate("A"), Candidate("B")) // result is from OpaVote's page - http://blog.opavote.com/2017/04/meek-stv-explained.html

  "MeekSTV Test " should {

    "verify result" in { meekSTVVerification("43-example.e", "43-candidates.txt") shouldEqual expectedMeekSTVWinnerList}
  }

  def meekSTVVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    MeekSTV.winners(election, candidates, 3).map {_._1}
  }
}
