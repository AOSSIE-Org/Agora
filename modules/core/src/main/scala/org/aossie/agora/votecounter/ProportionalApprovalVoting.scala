package org.aossie.agora.votecounter

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/** * https://en.wikipedia.org/wiki/Proportional_approval_voting
  */
object ProportionalApprovalVoting extends VoteCounter[Ballot] {

  // following function calculates score, i.e., given N, it calculates summation 1 to 1/N
  def proportionalApprovalScore(nmatches: Int): Rational = {
    var score = Rational(0, 1)
    for (i <- 1 to nmatches)
      score = score + Rational(1, i)
    score
  }

  // following function calculates totals for each candidate subsets in the follwoing manner
  // if N of the candidate preferences matches with any one candidate subset,
  // then score for that subset is summation 1 to 1/N
  def candidateSubsetTotals(
      election: Election[Ballot],
      candidates: List[Candidate],
      ccandSubsetList: List[List[Candidate]]
  ): List[(Candidate, Rational)] = {
    val scoredCandidateSubsetMap = new MMap[List[Candidate], Rational]
    for (a <- ccandSubsetList) {
      for (b <- election) {
        scoredCandidateSubsetMap(a) = scoredCandidateSubsetMap.getOrElse(a, Rational(0, 1)) +
          proportionalApprovalScore(
            b.preferences.length - b.preferences.toSet[Candidate].diff(a.toSet[Candidate]).size
          )
      }
    }
    val sortedCandidateSubsetList = scoredCandidateSubsetMap.toList.sortWith(_._2 > _._2)
    val winnerList                = sortedCandidateSubsetList.head._1
    val winnerScore               = sortedCandidateSubsetList.head._2
    val finalList = winnerList.map {
      (_, winnerScore)
    }
    finalList
  }

  // generates subsets of length k of list of candidates in recursive manner
  def candidateSubsetListGenerator(k: Int, candidates: List[Candidate]): List[List[Candidate]] = {
    candidates match {
      case Nil => Nil
      case head :: tail =>
        if (k <= 0 || k > candidates.length) {
          Nil
        } else if (k == 1) {
          candidates.map(List(_))
        } else {
          candidateSubsetListGenerator(k - 1, tail).map(head :: _) ::: candidateSubsetListGenerator(
            k,
            tail
          )
        }
    }
  }

  def winners(
      election: Election[Ballot],
      ccandidates: List[Candidate],
      numVacancies: Int
  ): List[(Candidate, Rational)] = {
    val ccandSubsetList = candidateSubsetListGenerator(numVacancies, ccandidates)
    candidateSubsetTotals(election, ccandidates, ccandSubsetList)
  }

}
