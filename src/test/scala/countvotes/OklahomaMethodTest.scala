
import countvotes.methods.OklahomaMethod
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate, Rational}
import org.specs2.mutable.Specification


class OklahomaMethodTest extends Specification {

  val expectedOklahomaMethodWinnerList = List((Candidate("Nashville"),Rational(173,3)))

  "OklahomaMethod Test " should {

    "verify result" in { OklahomaMethodVerification("14-example.txt", "14-candidates.txt") shouldEqual expectedOklahomaMethodWinnerList }
  }

  def OklahomaMethodVerification(electionFile: String, candidatesFile: String): List[(Candidate,Rational)] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    OklahomaMethod.winners(election, candidates, 1)
  }
}
