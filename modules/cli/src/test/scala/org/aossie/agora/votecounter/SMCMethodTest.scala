package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.ParameterParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

/** Created by deepeshpandey on 06/08/17. */
class SMCMethodTest extends Specification {

  val smcWinnerList = List(new Candidate("C"))

  "SMC Test " should {

    "verify result" in {
      smcMethodVerification(
        "37-example.e",
        "37-candidates.txt",
        "method-param1.json"
      ) shouldEqual smcWinnerList
    }
    "verify result" in {
      smcMethodVerification(
        "37-example.e",
        "37-candidates.txt",
        "method-param.json"
      ) shouldEqual smcWinnerList
    }
  }

  def smcMethodVerification(
      electionFile: String,
      candidateFile: String,
      paramFile: String
  ): List[Candidate] = {

    val dir        = "./files/Examples/"
    val candidates = CandidatesParser.read(dir + candidateFile)
    val election   = PreferencesParser.read(dir + electionFile)
    val param      = ParameterParser.parse(dir + paramFile)

    SMC.smcWinner(election, candidates, param, 1).map(_._1)
  }

}
