package org.aossie.agora.votecounter

import org.aossie.agora.model._

import spire.math.Rational

/** * https://en.wikipedia.org/wiki/Sequential_proportional_approval_voting
  */

object SequentialProportionalApprovalVoting
    extends VoteCounter[PreferenceBallot]
    with SimpleApproval {

  // following function removes winner and reduces weight on ballot to 1/(N+1)
  // where N is the number of winners in one single ballot choice list
  def excludeWinner[C <: Candidate](
      election: Election[C, PreferenceBallot],
      winner: (C, Rational)
  ): Election[C, PreferenceBallot] = {
    var ballots: List[PreferenceBallot[C]] = Nil
    for (b <- election) {
      if (b.preferences.contains(winner._1)) {
        ballots = new PreferenceBallot(
          b.preferences.filter(_ != winner._1),
          b.id,
          Rational(b.weight.numerator, b.weight.denominator + 1)
        ) :: ballots
      } else {
        ballots = new PreferenceBallot(b.preferences, b.id, b.weight) :: ballots
      }
    }
    Election(ballots)
  }

  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    var winnerList: List[(C, Rational)] = Nil
    var election1                       = election
    var ccandidates1                    = ccandidates
    var vacancies                       = numVacancies
    while (vacancies != 0) {
      val winner = countApprovals(election1, ccandidates1).toList.sortWith(_._2 > _._2).head
      winnerList = winner :: winnerList
      election1 = excludeWinner(election1, winner)
      ccandidates1 = ccandidates1.filter(_ != winner._1)
      vacancies = vacancies - 1
    }
    winnerList
  }

}
