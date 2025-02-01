package org.aossie.agora.votecounter;

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class MeekSTVTest extends Specification {

  val expectedMeekSTVWinnerList = List(
    new Candidate("C"),
    new Candidate("A"),
    new Candidate("B")
  ) // result is from OpaVote's page - http://blog.opavote.com/2017/04/meek-stv-explained.html

  "MeekSTV Test " should {

    "verify result" in {
      meekSTVVerification("43-example.e", "43-candidates.txt") shouldEqual expectedMeekSTVWinnerList
    }
  }

  def meekSTVVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    new MeekSTV().winners(election, candidates, 3).map(_._1)
  }

}
