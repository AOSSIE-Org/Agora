package countvotes.methods

import countvotes.structures._

/**
  * A proper definition of strategyproofness for irresolute SCFs requires the specification
  * of preferences over sets of alternatives. One way to obtain such preferences is to
  * extend the preferences that voters have over individual alternatives to (not necessarily
  * complete) preference relations over sets. A function that yields a preference relation
  * over subsets of alternatives when given a preference relation over single alternatives is
  * called a set extension. This implementation is for one of the most natural and well studied
  * set extension called - Fishburn's Extension.
  *
  * link : https://drive.google.com/file/d/0B4uPp6wWiMpSbWh2NGNfLXdiTTA/view?usp=sharing
  */

object FishburnsExtensionMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]


  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], param: Parameters,
                  numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(fishburnPreferredSet(election, candidates, param))

    report
  }

  def fishburnPreferredSet(election: Election[WeightedBallot], candidates: List[Candidate],
                           parameters: Parameters): List[(Candidate, Rational)] = {

    // candidates in comparison sets should be consistent with the actual candidates
    require(parameters.comparisonSets.isDefined && parameters.comparisonSets.get.set1.forall(c => candidates.exists(cand => cand.name == c)) &&
      parameters.comparisonSets.get.set2.forall(c => candidates.exists(cand => cand.name == c)))

    val setX = parameters.comparisonSets.get.set1.map(name => candidates.find(cand => cand.name == name).get).toSet
    val setY = parameters.comparisonSets.get.set2.map(name => candidates.find(cand => cand.name == name).get).toSet
    val matrix = getPairwiseComparison(election, candidates)

    if (fishburnComparison(matrix, setX, setY, candidates)) {
      setX.toList.map(c => (c, Rational(0, 1)))

    } else if(fishburnComparison(matrix, setY, setX, candidates)) {
      setY.toList.map(c => (c, Rational(0, 1)))

    } else {
      List()
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


  def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = ???
}
