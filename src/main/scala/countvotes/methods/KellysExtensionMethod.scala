package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.methods.CopelandMethod.{printElection, winners}
import countvotes.structures._

/**
  * A proper definition of strategyproofness for irresolute SCFs requires the specification
  * of preferences over sets of alternatives. One way to obtain such preferences is to
  * extend the preferences that voters have over individual alternatives to (not necessarily
  * complete) preference relations over sets. A function that yields a preference relation
  * over subsets of alternatives when given a preference relation over single alternatives is
  * called a set extension. This implementation is for one of the most natural and well studied
  * set extension called - kelly's Extension.
  *
  * link : https://drive.google.com/file/d/0B4uPp6wWiMpScEM2Q21kT2x1N3M/view?usp=sharing
  */
object KellysExtensionMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]


  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], param: Parameters,
                  numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(kellyPreferredSet(election, candidates, param, numVacancies))

    report
  }

  def kellyPreferredSet(election: Election[WeightedBallot], candidates: List[Candidate], param: Parameters,
                        numVacancies: Int): List[(Candidate, Rational)] = {

    // require that the sets are consistent with the candidates list
    require(param.comparisonSets.isDefined && param.comparisonSets.get.set1.forall(c => candidates.exists(cand => cand.name == c)) &&
      param.comparisonSets.get.set2.forall(c => candidates.exists(cand => cand.name == c)))

    logger.info("Computing Kelly preferred set")

    val majorityMatrix = getPairwiseComparison(election, candidates)
    val setX = param.comparisonSets.get.set1.map(name => candidates.find(cand => cand.name == name).get).toSet
    val setY = param.comparisonSets.get.set2.map(name => candidates.find(cand => cand.name == name).get).toSet

    if (kellyComparison(majorityMatrix, setX, setY, candidates)) {
      setX.toList.map(c => (c, Rational(0, 1)))
    } else if (kellyComparison(majorityMatrix, setY, setX, candidates)) {
      setY.toList.map(c => (c, Rational(0, 1)))
    } else {
      List()
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

  def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = ???
}
