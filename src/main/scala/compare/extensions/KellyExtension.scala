package compare.extensions

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

/*
  Link : https://drive.google.com/file/d/0B4uPp6wWiMpScEM2Q21kT2x1N3M/view?usp=sharing
 */
object KellyExtension extends SetExtensionMethods[WeightedBallot] with LazyLogging {


  override def compare(election: Election[WeightedBallot], candidates: List[Candidate], parameters: Parameters): Set[Candidate] = {
    // require that the sets are consistent with the candidates list
    require(parameters.comparisonSets.isDefined && parameters.comparisonSets.get.set1.forall(c => candidates.exists(cand => cand.name == c)) &&
      parameters.comparisonSets.get.set2.forall(c => candidates.exists(cand => cand.name == c)))

    logger.info("Computing Kelly preferred set")

    val majorityMatrix = getPairwiseComparisons(election, candidates)
    val setX = parameters.comparisonSets.get.set1.map(name => candidates.find(cand => cand.name == name).get).toSet
    val setY = parameters.comparisonSets.get.set2.map(name => candidates.find(cand => cand.name == name).get).toSet

    if (kellyComparison(majorityMatrix, setX, setY, candidates)) {
      println("\n\nKelly Preferred Set is " + setX.mkString(" , "))
      setX
    } else if (kellyComparison(majorityMatrix, setY, setX, candidates)) {
      println("\n\nKelly Preferred Set is " + setY.mkString(" , "))
      setY
    } else {
      Set()
    }

  }

  /**
    * X PK Y ⇔ ∀x∈X, y∈Y: (x R y) ∧ ∃x∈X, y∈Y: (x P y) => set X is Kelly preferred to set Y
    * @param matrix matrix of pairwise votes between candidates
    * @param setX
    * @param setY
    * @return
    */
  def kellyComparison(matrix: Array[Array[Rational]], setX: Set[Candidate], setY: Set[Candidate],
                      candidates: List[Candidate]): Boolean = {

    val xRy = setX.forall(cand1 => setY.forall(cand2 => {
      matrix(candidates.indexOf(cand1))(candidates.indexOf(cand2)) >= matrix(candidates.indexOf(cand2))(candidates.indexOf(cand1))
    }))

    xRy && setX.exists(x => setY.exists(y => matrix(candidates.indexOf(x))(candidates.indexOf(y)) > matrix(candidates.indexOf(y))(candidates.indexOf(x))))

  }

}
