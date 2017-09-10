import agora.votecounter.{Majority, RandomBallot}
import agora.parsers.{CandidatesParser, PreferencesParser}
import agora.structures.Candidate
import org.specs2.mutable.Specification


class RandomBallotTest extends Specification {

  val expectedRandomBallotWinnerList1 = List(Candidate("E"), Candidate("C"))
  val expectedRandomBallotWinnerList2 = List(Candidate("E"))
  val expectedRandomBallotWinnerList3 = List(Candidate("D"), Candidate("E"))
  val expectedRandomBallotWinnerList4 = List(Candidate("A"), Candidate("B"), Candidate("C"))
  val expectedRandomBallotWinnerList5 = List(Candidate("D"), Candidate("E"))
  val expectedRandomBallotWinnerList6 = List(Candidate("E"), Candidate("C"), Candidate("B"), Candidate("D"))


  "RandomBallot Test " should {

    "verify result" in { randomBallotMethodVerification("21-example.e", "21-candidates.txt", Option(6142), 2) shouldEqual expectedRandomBallotWinnerList1 }
    "verify result" in { randomBallotMethodVerification("21-example.e", "21-candidates.txt", Option(2416), 1) shouldEqual expectedRandomBallotWinnerList2 }
    "verify result" in { randomBallotMethodVerification("21-example.e", "21-candidates.txt", Option(1426), 2) shouldEqual expectedRandomBallotWinnerList3 }
    "verify result" in { randomBallotMethodVerification("21-example.e", "21-candidates.txt", Option(4216), 3) shouldEqual expectedRandomBallotWinnerList4 }
    "verify result" in { randomBallotMethodVerification("21-example.e", "21-candidates.txt", Option(1264), 2) shouldEqual expectedRandomBallotWinnerList5 }
    "verify result" in { randomBallotMethodVerification("21-example.e", "21-candidates.txt", Option(2614), 4) shouldEqual expectedRandomBallotWinnerList6 }

  }

  def randomBallotMethodVerification(electionFile: String, candidatesFile: String, seed: Option[Int], vacancies: Int): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    RandomBallot.randomBallotWinner(election, candidates, vacancies, seed).map {_._1}
  }

}
