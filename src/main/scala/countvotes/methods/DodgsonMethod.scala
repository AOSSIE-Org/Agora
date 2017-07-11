package countvotes.methods

import countvotes.structures._

/**
  * Wiki : https://en.wikipedia.org/wiki/Dodgson%27s_method
  * Implementation : http://infosyncratic.nl/talks/2008-votingprocedures.pdf
  */
object DodgsonMethod extends VoteCountingMethod[WeightedBallot] {

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  override def winners(e: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {

    val minDodgsonFlip = List.fill(Election.totalWeightedVoters(e).toInt)(0 to ccandidates.length)
      .flatten
      .combinations(Election.totalWeightedVoters(e).toInt)
      .flatMap(_.permutations)
      .toList
      .sortBy(_.sum)
      .find(list => getCondorcetWinnerIfExist(list, ccandidates, e).nonEmpty)

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

  def getCandidateMajorityArray(election: Election[WeightedBallot], candidate: Candidate, flipVector: List[Int],
                                candidates: List[Candidate]): Option[Array[Int]] = {

    val dispersedElection = getDispersedElection(election)

    assert(dispersedElection.length == flipVector.length)

    val candidateMajorityMatrix = Array.ofDim[Int](candidates.size)

    val succesful = dispersedElection zip flipVector forall (ballotswithFlip => {

      if (isFlippable(candidate, ballotswithFlip._2, ballotswithFlip._1.preferences)) {

        val prefs = ballotswithFlip._1.preferences
        for (cand <- prefs) {
          if ((prefs.indexOf(cand) >= prefs.indexOf(candidate) - ballotswithFlip._2) && candidate != cand) {
            candidateMajorityMatrix {candidates.indexOf(cand)} += 1
          }
        }
        true
      } else {
        false
      }
    })

    if (succesful) {
      Option(candidateMajorityMatrix)
    } else {
      None
    }
  }


  def getDispersedElection(election: Election[WeightedBallot]): Election[WeightedBallot] = {
    for {
      b <- election
      i <- 1 to b.weight.toInt
    } yield b
  }


  def isCondorcetWinner(candidate: Candidate, candidates: List[Candidate],
                        matrix: Array[Int], totalVoters: Int): Boolean =
    matrix.zipWithIndex.forall(c => c._1 > Rational(1, 2) * totalVoters || (c._2 == candidates.indexOf(candidate)))


  def isFlippable(candidate: Candidate, rank: Int, ballot: List[Candidate]): Boolean = if (ballot.indexOf(candidate) - rank >= 0) true else false

}