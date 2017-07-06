import countvotes.methods.{MajorityRuleMethod, RandomBallotMethod}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification


class RandomBallotTest extends Specification {

  val expectedRandomBallotWinnerList1 = List(Candidate("B"), Candidate("D"))
  val expectedRandomBallotWinnerList2 = List(Candidate("E"))
  val expectedRandomBallotWinnerList3 = List(Candidate("E"), Candidate("C"))
  val expectedRandomBallotWinnerList4 = List()
  val expectedRandomBallotWinnerList5 = List(Candidate("E"), Candidate("C"))
  val expectedRandomBallotWinnerList6 = List(Candidate("C"), Candidate("D"), Candidate("B"), Candidate("A"))


  "RandomBallot Test " should {

    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt", Option(6142), 2) shouldEqual expectedRandomBallotWinnerList1 }
    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt", Option(2416), 1) shouldEqual expectedRandomBallotWinnerList2 }
    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt", Option(1426), 2) shouldEqual expectedRandomBallotWinnerList3 }
    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt", Option(4216), 3) shouldEqual expectedRandomBallotWinnerList4 }
    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt", Option(1264), 2) shouldEqual expectedRandomBallotWinnerList5 }
    "verify result" in { randomBallotMethodVerification("16-example.txt", "16-candidates.txt", Option(2614), 4) shouldEqual expectedRandomBallotWinnerList6 }

  }

  def randomBallotMethodVerification(electionFile: String, candidatesFile: String, seed: Option[Int], vacancies: Int): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    RandomBallotMethod.randomBallotWinner(election, candidates, vacancies, seed).map {_._1}
  }

}
