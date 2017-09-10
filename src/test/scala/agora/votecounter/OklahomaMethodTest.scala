package agora.votecounter;

import agora.parser.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification

import spire.math.Rational

class OklahomaMethodTest extends Specification {

  val expectedOklahomaMethodWinnerList = List((Candidate("Nashville"),Rational(173,3)))

  "OklahomaMethod Test " should {

    "verify result" in { OklahomaMethodVerification("14-example.e", "14-candidates.txt") shouldEqual expectedOklahomaMethodWinnerList }
  }

  def OklahomaMethodVerification(electionFile: String, candidatesFile: String): List[(Candidate,Rational)] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Oklahoma.winners(election, candidates, 1)
  }
}
