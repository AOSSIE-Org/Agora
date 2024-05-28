package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import scala.collection.immutable.{Map => IMap}
import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/***
  * https://en.wikipedia.org/wiki/Sequential_proportional_approval_voting
  */


object SequentialProportionalApprovalVoting extends VoteCounter[Ballot]
  with SimpleApproval {

  // following function removes winner and reduces weight on ballot to 1/(N+1)
  // where N is the number of winners in one single ballot choice list
  def excludeWinner(election: Election[Ballot], winner: (Candidate, Rational)): Election[Ballot] = {
    var ballots: List[Ballot]  = Nil
    for(b<-election) {
      if(b.preferences.contains(winner._1)) {
        ballots = new Ballot(b.preferences.filter(_ != winner._1), b.id, Rational(b.weight.numerator, b.weight.denominator + 1)) :: ballots
      } else {
        ballots = new Ballot(b.preferences, b.id, b.weight) :: ballots
      }
    }
    Election(ballots)
  }

  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    var winnerList: List[(Candidate, Rational)] = Nil
    var election1 = election
    var ccandidates1 = ccandidates
    var vacancies = numVacancies
    while(vacancies != 0) {
      val winner = countApprovals(election1, ccandidates1).toList.sortWith(_._2 > _._2).head
      winnerList = winner :: winnerList
      election1 = excludeWinner(election1, winner)
      ccandidates1 = ccandidates1.filter(_ != winner._1)
      vacancies = vacancies - 1
    }
    winnerList
  }
}
