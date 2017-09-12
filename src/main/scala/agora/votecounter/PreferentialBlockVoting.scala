package agora.votecounter

import agora.votecounter.HybridPluralityPreferentialBlockVoting.exclude
import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/**
  * https://en.wikipedia.org/wiki/Preferential_block_voting
  */

object PreferentialBlockVoting extends VoteCounter[Ballot] {

  val majorityThreshold = Rational(1, 2)

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
    var winnerlist: List[(Candidate, Rational)] = Nil
    var vacancies = numVacancies
    var ccandidates1 = ccandidates
    var election1 = election
    while (vacancies != 0) {
      val sortedCandList = Election.firstVotes(election1, ccandidates1).toList.sortWith(_._2 > _._2)
      if (sortedCandList.head._2 > majorityThreshold * election1.length && ccandidates1.length > vacancies) {
        winnerlist = sortedCandList.head :: winnerlist
        vacancies -= 1
        ccandidates1 = ccandidates1.filter(_ != sortedCandList.head._1)
        election1 = exclude(election1, sortedCandList.head._1)
      } else if (ccandidates1.length == vacancies) {
        winnerlist = sortedCandList ::: winnerlist
        vacancies = 0
      } else {
        ccandidates1 = ccandidates1.filter(_ != sortedCandList.last._1)
        election1 = exclude(election1, sortedCandList.last._1)
      }
    }
    winnerlist
  }

}

