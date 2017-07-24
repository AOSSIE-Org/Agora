package countvotes.methods

import countvotes.structures._

import collection.mutable.{HashMap => MMap}
/**
  * Wiki : https://en.wikipedia.org/wiki/Dodgson%27s_method
  * Implementation : http://infosyncratic.nl/talks/2008-votingprocedures.pdf
  */
object DodgsonMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    require(election forall(b => b.weight.denominator == 1) )

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  override def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    // find flip vector from min sum to max sum which satisfies condorcet condition
    val minDodgsonFlip = List.fill(Election.totalWeightedVoters(e).toInt)(0 to ccandidates.length)
      .flatten
      .combinations(Election.totalWeightedVoters(e).toInt)
      .flatMap(_.permutations)
      .toList
      .sortBy(_.sum)
      .find(list => getCondorcetWinnerIfExist(list, ccandidates, e).nonEmpty)

    // find the dodgson winner based on this flip vector
    minDodgsonFlip match {
      case Some(list) => getCondorcetWinnerIfExist(list, ccandidates, e) match {
        case Some(candidate) => List((candidate, Rational(list.sum, 1)))
        case None => List()
      }
      case None => List()
    }
  }


  def getCondorcetWinnerIfExist(list: List[Int], candidates: List[Candidate], election: Election[WeightedBallot]): Option[Candidate] = {

    val dodgsonWinner = candidates.find(c => getCandidateMajorityArray(election, c, list, candidates) match {
      case Some(matrix) => isCondorcetWinner(c, candidates, matrix, Election.totalWeightedVoters(election).toInt)
      case None => false
    })

    dodgsonWinner
  }

  // returns an array where the value at index i represents total votes to param candidate against candidates(i)
  // this is all required to calculate if the param candidate is condorcet winner or not
  def getCandidateMajorityArray(election: Election[WeightedBallot], candidate: Candidate, flipVector: List[Int],
                                candidates: List[Candidate]): Option[Array[Int]] = {

    val dispersedElection = dispersed(election)

    assert(dispersedElection.length == flipVector.length)

    val candElectionResponse = Array.ofDim[Int](candidates.size)

    val succesful = dispersedElection zip flipVector forall (ballotsWithFlip => {

      if (isFlippable(candidate, ballotsWithFlip._2, ballotsWithFlip._1.preferences)) {

        // no need to generate new list with updated positions just compare using indices
        val prefs = ballotsWithFlip._1.preferences
        for (cand <- prefs) {
          if ((prefs.indexOf(cand) >= prefs.indexOf(candidate) - ballotsWithFlip._2) && candidate != cand) {
            candElectionResponse {candidates.indexOf(cand)} += 1
          }
        }
        true
      } else {
        false
      }
    })

    if (succesful) {
      Option(candElectionResponse)
    } else {
      None
    }
  }

  private val cache = new MMap[Election[WeightedBallot], Election[WeightedBallot]]()

  def dispersed(election: Election[WeightedBallot]): Election[WeightedBallot] = {

    def disperseUtil(election: Election[WeightedBallot]) = {
      for {
        b <- election
        i <- 1 to b.weight.toInt
      } yield b
    }
    cache.getOrElseUpdate(election, disperseUtil(election))
  }


  def isCondorcetWinner(candidate: Candidate, candidates: List[Candidate],
                        matrix: Array[Int], totalVoters: Int): Boolean =
    matrix.zip(candidates).forall {case (score, cand) => {score > Rational(1, 2) * totalVoters || (cand == candidate)}}


  def isFlippable(candidate: Candidate, rank: Int, ballot: List[Candidate]): Boolean = if (ballot.indexOf(candidate) - rank >= 0) true else false

}