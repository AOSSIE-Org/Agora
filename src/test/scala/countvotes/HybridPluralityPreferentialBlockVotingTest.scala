
import countvotes.methods.HybridPluralityPreferentialBlockVoting
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

class HybridPluralityPreferentialBlockVotingTest extends Specification {

  val expectedHybridPluralityPreferentialBlockVotingWinnerList = List(Candidate("Sue"))
  val expectedHybridPluralityPreferentialBlockVotingWinnerList1 = List(Candidate("Bill"), Candidate("Bob"))

  "HybridPluralityPreferentialBlockVoting Test " should {
    "verify result" in { HybridPluralityPreferentialBlockVotingVerification("24-example.e", "24-candidates.txt") shouldEqual expectedHybridPluralityPreferentialBlockVotingWinnerList }
    "verify result" in { HybridPluralityPreferentialBlockVotingVerification1("24-example.e", "24-candidates.txt") shouldEqual expectedHybridPluralityPreferentialBlockVotingWinnerList1 }
  }

  def HybridPluralityPreferentialBlockVotingVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    HybridPluralityPreferentialBlockVoting.winners(election, candidates, 1).map {_._1}
  }

  def HybridPluralityPreferentialBlockVotingVerification1(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    HybridPluralityPreferentialBlockVoting.winners(election, candidates, 2).map {_._1}
  }
}
