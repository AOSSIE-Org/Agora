package agora.votecounter

import agora.model._
import agora.model.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/***
  * https://en.wikipedia.org/wiki/Bucklin_voting
  * we are not enforcing a strict number of preferences per ballot, unlike in the original Bucklin method
  * so ballots with fewer preferences are not voided
  */

object Bucklin extends VoteCounter[Ballot]{

  // The following recursive function calculates totals and if total of any candidate exceeds half of election length
  // candidate wins, else next preferences are added, with weight 1/1 as previous preferences
  def bucklinTotals(election: Election[Ballot], ccandidates: List[Candidate], ccandScoreMap: MMap[Candidate, Rational]): List[(Candidate, Rational)] = {
    val candidateScoreMap = ccandScoreMap
    val candidateTotalScores = election.firstVotes(ccandidates)
    for (c<-ccandidates) {
      candidateScoreMap(c) = candidateScoreMap.getOrElse(c, Rational(0, 1)) + candidateTotalScores.getOrElse(c, Rational(0, 1)) * Rational(1,1)
    }
    val sortedCandidateScoreMap = candidateScoreMap.toList.sortWith(_._2 > _._2)
    if (sortedCandidateScoreMap.head._2 > (Rational(election.length, 2))) {
      sortedCandidateScoreMap.head :: List()
    } else {
      var ballots: List[Ballot] = Nil
      for (b<-election) {
        ballots = new Ballot(if (b.preferences != Nil) b.preferences.tail else Nil, b.id, b.weight) :: ballots
      }
      bucklinTotals(Election(ballots), ccandidates, candidateScoreMap)
    }
  }

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
    var ccandidateScoreMap = new MMap[Candidate, Rational]
    var winner = bucklinTotals(election, ccandidates, ccandidateScoreMap)
    winner
  }

}
