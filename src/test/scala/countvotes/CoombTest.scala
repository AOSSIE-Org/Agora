package countvotes

import agora.votecounter.Coomb
import agora.parsers.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer


class CoombTest extends Specification {

  val expectedCoombWinnerList = List(Candidate("Nashville"))
  val expectedCoombWinnerList1 = List(Candidate("B")) // tie resolution test case both A/B could be a winner here

  "Coomb Test" should {
    "verify result" in {
      coombMethodVerification("14-example.e", "14-candidates.txt") shouldEqual expectedCoombWinnerList
      coombMethodVerification("23-example.e", "23-candidates.txt") shouldEqual expectedCoombWinnerList1
    }
  }

  def coombMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election = PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    Coomb.winners(election, candidates, 1).map { _._1 }

  }
}
