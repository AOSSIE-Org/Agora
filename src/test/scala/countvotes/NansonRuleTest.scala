import countvotes.methods.{BordaRuleMethod, NansonRuleMethod}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.Candidate
import org.specs2.mutable.Specification

/**
  * Created by deepeshpandey on 19/06/17.
  */
class NansonRuleTest extends Specification{

  val expectedNansonWinnerList = List(Candidate("Nashville"))

  "Nanson Rule Test " should {

    "verify result" in { nansonRuleMethodVerification("14-example.txt") shouldEqual expectedNansonWinnerList }
  }

  def nansonRuleMethodVerification(electionFile: String): List[Candidate] = {

    val candidates = CandidatesParser.read("../Agora/files/Examples/14-candidates.txt")
    val election =  PreferencesParser.read("../Agora/files/Examples/" + electionFile)

    NansonRuleMethod.winners(election, candidates, 1).map {_._1}
  }
}
