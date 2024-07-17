package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParserWithRank
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** this class tests the example on https://en.wikipedia.org/wiki/Schulze_method on two preference
  * profiles 15-example.txt and 16-example.txt given in different format
  */
class SchulzeTest extends Specification {

  val expectedSchulzeWinnerList =
    List(new Candidate("E"), new Candidate("A"), new Candidate("C"), new Candidate("B"),
      new Candidate("D"))

  "Schulze Test" should {
    "verify result" in {
      schulzeMethodVerification(
        "44-example.er",
        "44-candidates.txt"
      ) shouldEqual expectedSchulzeWinnerList
    }
  }

  def schulzeMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val dir        = "./files/Examples/"
    val candidates = CandidatesParser.read(dir + candidatesFile)
    val election   = PreferencesParserWithRank.read(dir + electionFile)

    Schulze.winners(election, candidates, candidates.length).map(_._1)
  }

}
