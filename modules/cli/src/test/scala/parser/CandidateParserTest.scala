package parser

import org.aossie.agora.model.Candidate
import org.aossie.agora.parser.CandidatesParser
import org.specs2.mutable.Specification

object CandidateParserTest extends Specification {

  val expectedCandidateList: List[Candidate] =
    List(new Candidate("Apple"), new Candidate("Banana"), new Candidate("Cherry"))

  "CandidateParser Test " should {

    "verify result" in {
      candidatesParserVerification(
        "49-candidates.txt"
      ) mustEqual expectedCandidateList
    }

  }

  def candidatesParserVerification(candidatesFile: String): List[Candidate] =
    CandidatesParser.read("./files/Examples/" + candidatesFile)

}
