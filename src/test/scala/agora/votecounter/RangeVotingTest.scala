package agora.votecounter

import agora.parser.{CandidatesParser, PreferencesParserWithScore}
import agora.model.Candidate
import org.specs2.mutable.Specification


class RangeVotingTest extends Specification {

  val expectedRangeVotingWinnerList = List(Candidate("Nashville"))

  "RangeVoting Test " should {

    "verify result" in {
      rangeVotingMethod("42-example.es", "42-candidates.txt") shouldEqual expectedRangeVotingWinnerList
    }
  }

  def rangeVotingMethod(electionFile: String, candidatesFile: String): List[Candidate] = {

    val dir = "../Agora/files/Examples/"
    val candidates = CandidatesParser.read(dir + candidatesFile)
    val election = PreferencesParserWithScore.read(dir + electionFile)

    RangeVoting.winners(election, candidates, 1).map {
      _._1
    }
  }

}
