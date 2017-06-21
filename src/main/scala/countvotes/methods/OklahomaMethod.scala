package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => MMap}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._

import countvotes.methods.VoteCountingMethod

/***
  * https://en.wikipedia.org/wiki/Oklahoma_primary_electoral_system
  */

object OklahomaMethod extends VoteCountingMethod[WeightedBallot] {

  protected val result: Result = new Result
  protected val report: Report[WeightedBallot] = new Report[WeightedBallot]

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int):   Report[WeightedBallot]  = {



    print("\n INPUT ELECTION: \n")
    printElection(election)

    val tls = totals(election, candidates) // Here are totals of candidates also not OCCURING in the ballots

    result.addTotalsToHistory(tls)

    //report.setCandidates(getCandidates(election))  // Here are candidates OCCURING in the election
    report.setCandidates(candidates)  // Here are candidates also not OCCURING in the election

    report.newCount(Input, None, Some(election), Some(tls), None, None)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def oklahomaTotals(election: Election[WeightedBallot], ccandidates: List[Candidate], m: MMap[Candidate, Rational], multiplier: Rational): List[(Candidate, Rational)] ={
   var map = m
    val tls1 = totals(election, ccandidates)
    for(c<-ccandidates){
      map(c) = map.getOrElse(c,Rational(0,1)) + tls1.getOrElse(c,Rational(0,1))*multiplier
    }
    if(map.toList.sortWith(_._2>_._2).head._2 > (election.length/2)){
      map.toList.sortWith(_._2>_._2).head :: List()
    } else {
      var newElection: Election[WeightedBallot] = Nil
      for(b<-election if !b.preferences.isEmpty){
        newElection = WeightedBallot(b.preferences.tail, b.id,b.weight) :: newElection
      }
      oklahomaTotals(newElection,ccandidates, map, Rational(multiplier.numerator, multiplier.denominator+1))
    }
    }

  override def winners(election: Election[WeightedBallot], ccandidates: List[Candidate],  numVacancies: Int): List[(Candidate, Rational)] = {
    var m = new MMap[Candidate, Rational]
    var multiplier = Rational(1,1)
    for(c<- ccandidates){
    m(c) = Rational(0,1)
    }
    var tls = oklahomaTotals(election, ccandidates, m, multiplier)
    tls
  }
}
