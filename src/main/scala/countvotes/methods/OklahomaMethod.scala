package countvotes.methods


import countvotes.structures._

import scala.collection.mutable.{HashMap => MMap}

/***
  * https://en.wikipedia.org/wiki/Oklahoma_primary_electoral_system
  * we are not enforcing a strict number of preferences per ballot, unlike in the original Oklahoma method
  * so ballots with fewer prefererences are not voided
  */

object OklahomaMethod extends Scrutiny[Ballot] {

  // following recursive function calculates totals and if total of any candidate exceeds half of election length
  // candidate wins, else next preferences are added, reducing their weights by 1/N,
  // where N denotes Nth preference on the ballot
  // that is, 1st preference has weight 1, 2nd preference has weight 1/2. 3rd preference has weight 1/3 and so on
  def oklahomaTotals(election: Election[Ballot], ccandidates: List[Candidate], ccandScoreMap: MMap[Candidate, Rational], multiplier: Rational): List[(Candidate, Rational)] = {
    val candidateScoreMap = ccandScoreMap
    val candidateTotalScores = totals(election, ccandidates)
    for (c<-ccandidates) {
      candidateScoreMap(c) = candidateScoreMap.getOrElse(c, Rational(0, 1)) + candidateTotalScores.getOrElse(c, Rational(0, 1)) * multiplier
    }
    val sortedCandidateScoreMap = candidateScoreMap.toList.sortWith(_._2 > _._2)
    if (sortedCandidateScoreMap.head._2 > (Rational(election.length, 2))) {
      sortedCandidateScoreMap.head :: List()
    } else {
      var newElection: Election[Ballot] = Nil
      for (b<-election) {
        newElection = new Ballot(if (b.preferences != Nil) b.preferences.tail else Nil, b.id, b.weight) :: newElection
      }
      oklahomaTotals(newElection, ccandidates, candidateScoreMap, Rational(multiplier.numerator, multiplier.denominator + 1))
    }
  }

  override def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
    var ccandidateScoreMap = new MMap[Candidate, Rational]
    var winner = oklahomaTotals(election, ccandidates, ccandidateScoreMap, Rational(1,1))
    winner
  }
}
