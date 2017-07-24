package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => MMap}
import scala.collection.SortedMap
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

import countvotes.methods.VoteCountingMethod


/**
  * https://en.wikipedia.org/wiki/Preferential_block_voting
  */

object HybridPluralityPreferentialBlockVoting extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {


    print("\n INPUT ELECTION: \n")
    printElection(election)

    val tls = totals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots

    result.addTotalsToHistory(tls)

    //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates) // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def totals1(election: Election[WeightedBallot],ccandidates: List[Candidate], numVacancies: Int): MMap[Candidate,Rational] = {
    var m = new MMap[Candidate, Rational]
    for(c<-ccandidates){
      m(c)=Rational(0,1)
    }
    for(b<-election if !b.preferences.isEmpty){
      for(c<-b.preferences.take(numVacancies)){
        m(c) = m.getOrElse(c, Rational(0,1)) + b.weight
      }
    }
    m
  }

  def exclude(election: Election[WeightedBallot],ccandidate: Candidate) : Election[WeightedBallot] ={
    var list: Election[WeightedBallot] = Nil
    for(b<-election if !b.preferences.isEmpty){
      list = WeightedBallot(b.preferences.filter(_!=ccandidate), b.id, b.weight) :: list
    }
    list
  }

  def winnerList(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int, winnerlist: List[(Candidate, Rational)]): List[(Candidate, Rational)] ={
    var winnerlist1: List[(Candidate, Rational)] = winnerlist
    if(numVacancies > 0)
    {
      var tls = totals1(election, ccandidates, numVacancies)
      if(tls.toList.sortWith(_._2 > _._2).head._2 > (election.size / 2)) {
        winnerlist1 = tls.toList.sortWith(_._2>_._2).head :: winnerlist1
        var newElection = exclude(election,tls.toList.sortWith(_._2>_._2).head._1)
        winnerList(newElection, ccandidates.filter(_!=tls.toList.sortWith(_._2>_._2).head._1), numVacancies-1, winnerlist1)
      } else {
        var newElection = exclude(election,tls.filter(x => ccandidates.contains(x._1)).toList.sortWith(_._2<_._2).head._1)
        winnerList(newElection, ccandidates.filter(_!=tls.toList.sortWith(_._2<_._2).head._1), numVacancies, winnerlist1)
      }
    } else {
      winnerlist
    }
  }
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)] = {
   winnerList(election, ccandidates, numVacancies, Nil)
  }
}


