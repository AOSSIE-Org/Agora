
import countvotes.methods.PreferentialBlockVoting
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

class PreferentialBlockVotingTest extends Specification {

  val expectedPreferentialBlockVotingWinnerList = List(Candidate("Sue"))

  "PreferentialBlockVoting Test " should {

    "verify result" in { PreferentialBlockVotingVerification("32-example.e", "32-candidates.txt") shouldEqual expectedPreferentialBlockVotingWinnerList }
  }

  def PreferentialBlockVotingVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    PreferentialBlockVoting.winners(election, candidates, 1).map {_._1}
  }
}
