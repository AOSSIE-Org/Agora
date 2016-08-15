package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}


trait ACTNewWinnersDuringExclusion extends ACT{
  def declareNewWinnersWhileExcluding(candidate: Candidate, exhaustedBallots: Set[ACTBallot], newtotals: Map[Candidate, Rational], totalsWithoutNewWinners: Map[Candidate, Rational], newElectionWithoutFractionInTotals: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var newws: List[(Candidate,Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota) ) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, Some(extractMarkings(newElectionWithoutFractionInTotals))) 
      //------------ Reporting ------------------------------------------
      report.newCount(Exclusion, Some(candidate), Some(newElectionWithoutFractionInTotals), Some(newtotals), Some(newws), Some(exhaustedBallots))
    }
    //------------ Reporting ------------------------------------------
    else report.newCount(Exclusion, Some(candidate), Some(newElectionWithoutFractionInTotals), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    newws
  }
}

trait NoNewWinnersDuringExclusion extends ACT{
  def declareNewWinnersWhileExcluding(candidate: Candidate, exhaustedBallots: Set[ACTBallot], newtotals: Map[Candidate, Rational], totalsWithoutNewWinners: Map[Candidate, Rational], newElectionWithoutFractionInTotals: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    report.newCount(Exclusion, Some(candidate), Some(newElectionWithoutFractionInTotals), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    Nil
  }
}