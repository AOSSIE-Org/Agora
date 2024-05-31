package agora.analyzer

import agora.parser.CandidatesParser
import agora.parser.PreferencesParser
import org.specs2.mutable.Specification

/** Examples 1,2,3,4 from http://www.lamsade.dauphine.fr/~lang/papers/elo-ecai08.pdf */
class SinglePeakednessTest extends Specification {

  val singlePeaked    = true
  val notSinglePeaked = false

  "Single Peakedness Test " should {

    "verify result" in {
      singlePeakednessVerification("24-example.e", "24-candidates.txt") shouldEqual singlePeaked
    }
    "verify result" in {
      singlePeakednessVerification("25-example.e", "24-candidates.txt") shouldEqual notSinglePeaked
    }
    "verify result" in {
      singlePeakednessVerification("26-example.e", "24-candidates.txt") shouldEqual singlePeaked
    }
    "verify result" in {
      singlePeakednessVerification("27-example.e", "27-candidates.txt") shouldEqual notSinglePeaked
    }
    "verify result" in {
      singlePeakednessVerification("28-example.e", "27-candidates.txt") shouldEqual notSinglePeaked
    }
  }

  def singlePeakednessVerification(electionFile: String, candidatesFile: String): Boolean = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    SinglePeakAnalyser.analyse(election, candidates)
  }

}
