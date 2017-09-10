package countvotes


import ammonite.ops._
import org.specs2.mutable.Specification

import agora.Main;
import scala.collection.mutable

/**
  * This class executes all vote counting methods.
  * Its purpose is merely to check that Agora compiles and 
  * does not throw any obvious run-time exceptions.
  *
  */
class RunAllMethods extends Specification {

  def test(method: String, electionFile: String, candidatesFile: String): Boolean = {
    Main.main(Seq("-d", "files/Examples/", "-b", electionFile, "-c", candidatesFile, "-v", "2", "-m", method).toArray)
    return true
  }
  
  def str(i: Int) = if (i < 10) "0" + i else i.toString
  
  def run(method: String): Boolean = {
    (1 to 13) map { i => str(i) } map {
      i: String => test(method, s"$i-example.txt",s"$i-candidates.txt")
    } reduce { (b1,b2) => b1 && b2}
  }

  "Agora" should {
    "execute EVACS" in { test("EVACS", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute EVACSnoLP" in { test("EVACSnoLP", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute EVACSDWD" in { test("EVACSDWD", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute Senate" in { test("Senate", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute Simple" in { test("Simple", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute Egalitarian" in { test("Egalitarian", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute Majority" in { test("Majority", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute Approval" in { test("Approval", "02-example.e", "02-candidates.txt") shouldEqual true }
    "execute Borda" in { test("Borda", "13-example.e", "13-candidates.txt") shouldEqual true }
    "execute Baldwin" in { test("Baldwin", "13-example.e", "13-candidates.txt") shouldEqual true }
    "execute Nanson" in { test("Nanson", "13-example.e", "13-candidates.txt") shouldEqual true }
    "execute Kemeny-Young" in { test("Kemeny-Young", "14-example.e", "14-candidates.txt") shouldEqual true }
    "execute Coomb" in { test("Coomb", "14-example.e", "14-candidates.txt") shouldEqual true }
    "execute Contingent" in { test("Contingent", "14-example.e", "14-candidates.txt") shouldEqual true }
    "execute InstantExhaustiveBallot" in { test("InstantExhaustiveBallot", "14-example.e", "14-candidates.txt") shouldEqual true }
    "execute RandomBallot" in { test("RandomBallot", "21-example.e", "21-candidates.txt") shouldEqual true }
    "execute PreferentialBlockVoting" in { test("PreferentialBlockVoting", "32-example.e", "32-candidates.txt") shouldEqual true }
    "execute HybridPluralityPreferentialBlockVoting" in { test("HybridPluralityPreferentialBlockVoting", "32-example.e", "32-candidates.txt") shouldEqual true }
  }
}

