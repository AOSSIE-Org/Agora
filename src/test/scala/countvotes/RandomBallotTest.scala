import countvotes.methods.{MajorityRuleMethod, RandomBallotMethod}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

/**
  * Created by deepeshpandey on 12/06/17.
  */
class RandomBallotTest extends Specification {

  val expectedRandomBallotWinnerList = List(Candidate("B"), Candidate("D"), Candidate("C"), Candidate("E"), Candidate("A"))


  "RandomBallot Test " should {

    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt") shouldEqual expectedRandomBallotWinnerList }

  }

  def randomBallotMethodVerification(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    RandomBallotMethod.randomBallotWinner(election, candidates, candidates.length, false).map {_._1}
  }

  // to get the expected list for any other election file - run this method
  /*def expectedRandomBallotWinner(electionFile: String, candidatesFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    val expectedAnswer = RandomBallotMethod.randomBallotWinner(election, candidates, candidates.length, false).map {_._1}

    expectedAnswer

  }*/


}
