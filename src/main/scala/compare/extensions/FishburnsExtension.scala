package compare.extensions

import countvotes.structures._

/*
  Link : https://drive.google.com/file/d/0B4uPp6wWiMpSbWh2NGNfLXdiTTA/view?usp=sharing
 */
object FishburnsExtension extends SetExtensionMethods[Ballot] {

  override def compare(election: Election[Ballot], candidates: List[Candidate], parameters: Parameters): Set[Candidate] = {

    // candidates in comparison sets should be consistent with the actual candidates
    require(parameters.comparisonSets.isDefined && parameters.comparisonSets.get.set1.forall(c => candidates.exists(cand => cand.name == c)) &&
      parameters.comparisonSets.get.set2.forall(c => candidates.exists(cand => cand.name == c)))

    val setX = parameters.comparisonSets.get.set1.map(name => candidates.find(cand => cand.name == name).get).toSet
    val setY = parameters.comparisonSets.get.set2.map(name => candidates.find(cand => cand.name == name).get).toSet
    val matrix = getPairwiseComparisons(election, candidates)

    if (fishburnComparison(matrix, setX, setY, candidates)) {
      println("\n\nFishburn Preferred Set is " + setX.mkString(" , "))
      setX
    } else if(fishburnComparison(matrix, setY, setX, candidates)) {
      println("\n\nFishburn Preferred Set is " + setY.mkString(" , "))
      setY
    } else {
      Set()
    }
  }

  /**
    * X RF Y ⇔ (∀x∈X\Y, y∈Y: x R y) ∧ (∀x∈X, y∈Y\X: x R y)
    * @param matrix
    * @param setX
    * @param setY
    * @return
    */
  def fishburnComparison(matrix: Array[Array[Rational]], setX: Set[Candidate], setY: Set[Candidate], candidates: List[Candidate]): Boolean = {

    (setX -- setY).forall(x => setY.forall(y => {
      matrix(candidates.indexOf(x))(candidates.indexOf(y)) >= matrix(candidates.indexOf(y))(candidates.indexOf(x))
    })) && setX.forall(x => (setY -- setX).forall(y => {
      matrix(candidates.indexOf(x))(candidates.indexOf(y)) >= matrix(candidates.indexOf(y))(candidates.indexOf(x))
    }))
  }

}
