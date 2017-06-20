import countvotes.methods.ProportionalApprovalVoting
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate,Rational}
import org.specs2.mutable.Specification


class ProportionalApprovalVotingTest extends Specification {

  val expectedProportionalApprovalWinnerList = List((Candidate("C"), Rational(61,2)), (Candidate("A"), Rational(61,2)))

  "ProportionalApprovalVoting Test " should {

    "verify result" in { ProportionalApprovalVotingVerification("19-example.txt", "19-candidates.txt") shouldEqual expectedProportionalApprovalWinnerList}
  }

  def ProportionalApprovalVotingVerification(electionFile: String, candidatesFile: String): List[(Candidate,Rational)] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    ProportionalApprovalVoting.winners(election, candidates, 2)
  }
}
