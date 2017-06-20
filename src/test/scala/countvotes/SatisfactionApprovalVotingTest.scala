
import countvotes.methods.SatisfactionApprovalVoting
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate,Rational}
import org.specs2.mutable.Specification


class SatisfactionApprovalVotingTest extends Specification {

  val expectedSatisfactionApprovalWinnerList = List((Candidate("D"), Rational(6,1)), (Candidate("C"), Rational(6,1)))

  "SatisfactionApprovalVoting Test " should {

    "verify result" in { SatisfactionApprovalVotingVerification("18-example.txt", "18-candidates.txt") shouldEqual expectedSatisfactionApprovalWinnerList}
  }

  def SatisfactionApprovalVotingVerification(electionFile: String, candidatesFile: String): List[(Candidate,Rational)] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    SatisfactionApprovalVoting.winners(election, candidates, 2)
  }
}