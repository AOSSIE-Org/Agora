import countvotes.methods.{CopelandMethod, InstantRunoff2Round}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

/**
  * Created by deepeshpandey on 27/06/17.
  */
class CopelandTest extends Specification {
  val expectedRunoff2RoundMethodWinnerList = List(Candidate("Nashville"))

  "Copeland Test " should {

    "verify result" in { copelandMethodVerification("14-example.e", "14-candidates.txt") shouldEqual expectedRunoff2RoundMethodWinnerList }
  }

  def copelandMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    CopelandMethod.winners(election, candidates, 1).map {_._1}
  }

}
