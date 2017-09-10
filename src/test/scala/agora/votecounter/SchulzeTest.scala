package agora.votecounter

import agora.votecounter.Schulze
import agora.parsers.{CandidatesParser, PreferencesParserWithRank}
import agora.structures.Candidate
import org.specs2.mutable.Specification


/**
  * this class tests the example on https://en.wikipedia.org/wiki/Schulze_method
  * on two preference profiles 15-example.txt and 16-example.txt given in different format
  */
class SchulzeTest extends Specification {

  val expectedSchulzeWinnerList = List(Candidate("E"), Candidate("A"), Candidate("C"), Candidate("B"), Candidate("D"))

  "Schulze Test" should {
    "verify result" in {
      schulzeMethodVerification("44-example.er", "44-candidates.txt") shouldEqual expectedSchulzeWinnerList
    }
  }

  def schulzeMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val dir = "../Agora/files/Examples/"
    val candidates = CandidatesParser.read(dir + candidatesFile)
    val election = PreferencesParserWithRank.read(dir + electionFile)

    Schulze.winners(election, candidates, candidates.length).map { _._1 }
  }

}
