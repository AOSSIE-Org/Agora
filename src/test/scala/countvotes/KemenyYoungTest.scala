package countvotes

import org.specs2.mutable.Specification

/**
  * This test is to verify the election results with the wikipedia example https://en.wikipedia.org/wiki/Kemeny–Young_method
  */
class KemenyYoungTest extends Specification{

  def test(method: String, electionFile: String, candidatesFile: String): Boolean = {
    Main.main(Seq("-d", "files/Examples/", "-b", electionFile, "-c", candidatesFile, "-v", "2", "-m", method).toArray)
    return true
  }

  // https://en.wikipedia.org/wiki/Kemeny–Young_method rankings are Nashville > Chattanooga > Kknxville > Memphis

  "KemenyYoung Test " should {

    "execute Kemeny-young" in { test("Kemeny-Young", "14-example.txt", "14-candidates.txt") shouldEqual true }

  }
}
