package agora.votecounter.stv

import agora.structures._
import agora.votecounter._
import collection.mutable.{HashMap => Map}

import spire.math.Rational

// for ACT newElection is newElectionWithoutFractionInTotals
trait ACTNewWinnersDuringExclusion extends ACT{
  def declareNewWinnersWhileExcluding(candidate: Candidate,
                                      exhaustedBallots: Set[ACTBallot],
                                      newtotals: Map[Candidate, Rational],
                                      totalsWithoutNewWinners: Map[Candidate, Rational],
                                      newElection: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var newws: List[(Candidate,Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota) ) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, Some(extractMarkings(newElection)))
      //------------ Reporting ------------------------------------------
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(newtotals), Some(newws), Some(exhaustedBallots))
    }
    //------------ Reporting ------------------------------------------
    else {
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    }
    newws
  }
}

// Like ACT, but no markings
trait SenateNewWinnersDuringExclusion extends STV[ACTBallot]{
  
  val result: Result
  val report: Report[ACTBallot]
  
  def declareNewWinnersWhileExcluding(candidate: Candidate,
                                      exhaustedBallots: Set[ACTBallot],
                                      newtotals: Map[Candidate, Rational],
                                      totalsWithoutNewWinners: Map[Candidate, Rational],
                                      newElection: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    var newws: List[(Candidate,Rational)] = List()
    if (quotaReached(totalsWithoutNewWinners, result.getQuota)) {
      newws = returnNewWinners(totalsWithoutNewWinners, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, None)
      //------------ Reporting ------------------------------------------
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(newtotals), Some(newws), Some(exhaustedBallots))
    }
    //------------ Reporting ------------------------------------------
    else {
      report.newCount(Exclusion, Some(candidate), Some(newElection), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    }
    newws
  }
}


trait NoNewWinnersDuringExclusion extends ACT{
  def declareNewWinnersWhileExcluding(candidate: Candidate,
                                      exhaustedBallots: Set[ACTBallot],
                                      newtotals: Map[Candidate, Rational],
                                      totalsWithoutNewWinners: Map[Candidate, Rational],
                                      newElectionWithoutFractionInTotals: Election[ACTBallot]):  List[(Candidate,Rational)] = {
    report.newCount(Exclusion, Some(candidate), Some(newElectionWithoutFractionInTotals), Some(totalsWithoutNewWinners), None, Some(exhaustedBallots))
    Nil
  }
}
