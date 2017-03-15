package countvotes

import countvotes.methods.CoombRuleMethod
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer

/**
  * Created by deepeshpandey on 15/03/17.
  */
class CoombTest extends Specification{


  "Coomb Test" should {
    "verify result" in { coombMethodVerification("18-example.txt", "18-candidates.txt") shouldEqual true }
    //"verify result" in { coombMethodVerification("19-example.txt", "19-candidates.txt") shouldEqual true }


  }

  def coombMethodVerification(electionFile: String, candidatesFile: String): Boolean = {

    val candidates = CandidatesParser.read("/Users/deepeshpandey/Desktop/aossie/agora/Agora/files/Examples/" + candidatesFile)
    val election =  PreferencesParser.read("/Users/deepeshpandey/Desktop/aossie/agora/Agora/files/Examples/" + electionFile)
    val methodWinnersList = CoombRuleMethod.winners(election, candidates, 1)
    val correctWinnerResult = getCoombWinnerList(candidates)

    var matchSize = methodWinnersList.zip(correctWinnerResult).filter(x => {x._1._1 == x._2}).size

    matchSize == correctWinnerResult.length

  }

  def getCoombWinnerList(candidates: List[Candidate]): List[(Candidate)] = {
    // correct rankings from https://en.wikipedia.org/wiki/Schulze_method
    val coombsWinnerList = new ListBuffer[Candidate]

    coombsWinnerList.insert(0, new Candidate("Nashville"))

    coombsWinnerList.toList
  }





}
