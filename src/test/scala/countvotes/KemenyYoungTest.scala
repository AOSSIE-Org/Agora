package countvotes

import countvotes.methods.KemenyYoungMethod
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer

/**
  * This test is to verify the election results with the wikipedia example https://en.wikipedia.org/wiki/Kemeny–Young_method
  */
class KemenyYoungTest extends Specification{

  // https://en.wikipedia.org/wiki/Kemeny–Young_method rankings are Nashville > Chattanooga > Kknxville > Memphis

  "KemenyYoung Test " should {

    "verify result" in { kemenyYoungMethodVerification("14-example.txt") shouldEqual true }

  }

  def kemenyYoungMethodVerification(electionFile: String): Boolean = {

    val candidates = CandidatesParser.read("/Users/deepeshpandey/Desktop/aossie/agora/Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("/Users/deepeshpandey/Desktop/aossie/agora/Agora/files/Examples/" + electionFile)
    val winnersfile = "/Users/deepeshpandey/Desktop/aossie/agora/Agora/files/Examples/" + "winners/" + "Winners_" + "Schulze" + "_InputFile_" + electionFile
    val reportfile = "/Users/deepeshpandey/Desktop/aossie/agora/Agora/files/Examples/" + "reports/" + "Report_" + "Schulze" + "_InputFile_" + electionFile
    val methodWinnersList = KemenyYoungMethod.winners(election, candidates, 1)
    val correctWinnerResult = getKemenyYoungWinnerList(candidates)

    var matchSize = methodWinnersList.zip(correctWinnerResult).filter(x => {x._1._1 == x._2}).size

    matchSize == candidates.length

  }

  def getKemenyYoungWinnerList(candidates: List[Candidate]): List[(Candidate)] = {
    // correct rankings from https://en.wikipedia.org/wiki/Kemeny–Young_method
    val kemenyYoungWinnerList = new ListBuffer[Candidate]

    kemenyYoungWinnerList.insert(0, new Candidate("Nashville"))
    kemenyYoungWinnerList.insert(1, new Candidate("Chattanooga"))
    kemenyYoungWinnerList.insert(2, new Candidate("Knoxville"))
    kemenyYoungWinnerList.insert(3, new Candidate("Memphis"))

    kemenyYoungWinnerList.toList
  }


}
