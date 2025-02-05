package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._
import org.aossie.agora.votecounter.common.PreferencePairwiseComparison

import spire.math.Rational

/** Created by deepeshpandey on 03/06/17. About : Sequential Majority Comparison (SMC): Fix some
  * enumeration {x1, x2, . . . , xm} of the alternatives. The winner of round 1 is x1; the winner of
  * round i + 1 is the winner w of round i, if w >(majority) xi+1, and is xi+1, if xi+1 >(majority)
  * w; and the ultimate winner is the winner of round m.
  */
object SMC
    extends VoteCounter[PreferenceBallot]
    with PreferencePairwiseComparison[PreferenceBallot]
    with LazyLogging {

  def runVoteCounter(
      election: Election[Candidate, PreferenceBallot],
      candidates: List[Candidate],
      param: Parameters,
      numVacancies: Int
  ): Report[Candidate, PreferenceBallot] = {

    val result: Result[Candidate]                   = new Result
    val report: Report[Candidate, PreferenceBallot] = new Report

    print("\n INPUT ELECTION: \n")
    // printElection(election)

    report.setCandidates(candidates)

    report.setWinners(smcWinner(election, candidates, param, numVacancies))

    report
  }

  def smcWinner(
      election: Election[Candidate, PreferenceBallot],
      ccandidates: List[Candidate],
      param: Parameters,
      numVacancies: Int
  ): List[(Candidate, Rational)] = {

    // it may be possible that param candidates and actual candidates are inconsistent
    require(
      param.comparisonOrder.isDefined && param.comparisonOrder.get.forall(c =>
        ccandidates.exists(cand => cand.name == c)
      )
    )

    val zeroRational     = Rational(0, 1)
    val majorityRational = Rational(1, 2)

    val totalVoters      = election.weight
    val electionResponse = pairwiseComparison(election, ccandidates)

    // generate the ordered list of candidates
    val candOrderList =
      param.comparisonOrder.get.map(name => ccandidates.find(cand => cand.name == name).get)

    List(candOrderList.tail.foldLeft(candOrderList.head)((cA, cB) => {
      if (
        electionResponse(ccandidates.indexOf(cA))(
          ccandidates.indexOf(cB)
        ) > majorityRational * totalVoters
      ) cA
      else cB
    })).map(c => (c, zeroRational))

  }

  override def winners[C <: Candidate](
      e: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = ???

}
