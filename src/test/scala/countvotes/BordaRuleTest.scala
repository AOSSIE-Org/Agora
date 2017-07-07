import countvotes.methods.{BordaRuleMethod, KemenyYoungMethod}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

/**
  * Created by deepeshpandey on 19/06/17.
  */
class BordaRuleTest extends Specification{

  val expectedBordaWinnerList = List(Candidate("Nashville"), Candidate("Chattanooga"), Candidate("Memphis"), Candidate("Knoxville"))

  "Borda Rule Test " should {

    "verify result" in { bordaRuleMethodVerification("14-example.txt") shouldEqual expectedBordaWinnerList }
  }

  def bordaRuleMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    BordaRuleMethod.winners(election, candidates, candidates.length).map {_._1}
  }
}
