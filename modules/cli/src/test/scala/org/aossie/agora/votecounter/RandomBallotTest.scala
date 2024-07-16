package org.aossie.agora.votecounter

import org.aossie.agora.parser.CandidatesParser
import org.aossie.agora.parser.PreferencesParser
import org.aossie.agora.model.Candidate
import org.specs2.mutable.Specification

class RandomBallotTest extends Specification {

  val expectedRandomBallotWinnerList1 = List(new Candidate("E"), new Candidate("C"))
  val expectedRandomBallotWinnerList2 = List(new Candidate("E"))
  val expectedRandomBallotWinnerList3 = List(new Candidate("D"), new Candidate("E"))
  val expectedRandomBallotWinnerList4 =
    List(new Candidate("A"), new Candidate("B"), new Candidate("C"))
  val expectedRandomBallotWinnerList5 = List(new Candidate("D"), new Candidate("E"))
  val expectedRandomBallotWinnerList6 =
    List(new Candidate("E"), new Candidate("C"), new Candidate("B"), new Candidate("D"))

  "RandomBallot Test " should {

    "verify result" in {
      randomBallotMethodVerification(
        "21-example.e",
        "21-candidates.txt",
        Option(6142),
        2
      ) shouldEqual expectedRandomBallotWinnerList1
    }
    "verify result" in {
      randomBallotMethodVerification(
        "21-example.e",
        "21-candidates.txt",
        Option(2416),
        1
      ) shouldEqual expectedRandomBallotWinnerList2
    }
    "verify result" in {
      randomBallotMethodVerification(
        "21-example.e",
        "21-candidates.txt",
        Option(1426),
        2
      ) shouldEqual expectedRandomBallotWinnerList3
    }
    "verify result" in {
      randomBallotMethodVerification(
        "21-example.e",
        "21-candidates.txt",
        Option(4216),
        3
      ) shouldEqual expectedRandomBallotWinnerList4
    }
    "verify result" in {
      randomBallotMethodVerification(
        "21-example.e",
        "21-candidates.txt",
        Option(1264),
        2
      ) shouldEqual expectedRandomBallotWinnerList5
    }
    "verify result" in {
      randomBallotMethodVerification(
        "21-example.e",
        "21-candidates.txt",
        Option(2614),
        4
      ) shouldEqual expectedRandomBallotWinnerList6
    }

  }

  def randomBallotMethodVerification(
      electionFile: String,
      candidatesFile: String,
      seed: Option[Int],
      vacancies: Int
  ): List[Candidate] = {

    val candidates = CandidatesParser.read("./files/Examples/" + candidatesFile)
    val election   = PreferencesParser.read("./files/Examples/" + electionFile)

    RandomBallot.randomBallotWinner(election, candidates, vacancies, seed).map(_._1)
  }

}
