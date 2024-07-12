package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** This test is to verify the election results with the wikipedia example
  * https://en.wikipedia.org/wiki/Kemeny–Young_method
  */
class KemenyYoungTest extends Specification {

  val expectedKemenyYoungWinnerList = List(
    new Candidate("Nashville"),
    new Candidate("Chattanooga"),
    new Candidate("Knoxville"),
    new Candidate("Memphis")
  )

  "KemenyYoung Test " should {

    "verify result" in {
      kemenyYoungMethodVerification("14-example.e") shouldEqual expectedKemenyYoungWinnerList
    }
  }

  def kemenyYoungMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    KemenyYoung.winners(election, candidates, candidates.length).map(_._1)
  }

}
