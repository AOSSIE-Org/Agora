import countvotes.methods.InstantExhaustiveBallot
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification


class InstantExhaustiveBallotTest extends Specification {

  val expectedInstantExhaustiveBallotWinnerList = List(Candidate("Knoxville"))

  "InstantExhaustiveBallot Test " should {

    "verify result" in { InstantExhaustiveBallotVerification("14-example.e", "14-candidates.txt") shouldEqual expectedInstantExhaustiveBallotWinnerList }
  }

  def InstantExhaustiveBallotVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    InstantExhaustiveBallot.winners(election, candidates, 1).map {_._1}
  }
}
