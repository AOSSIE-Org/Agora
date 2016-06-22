package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._


import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._


abstract class GenericSTVMethod[B <: Ballot with Weight] {
  //type E = Election[B]
  
  
  // GLOBAL MUTABLE VARIABLE
  // DON'T FORGET TO RESET
  val result: Result = new Result
  
  def runScrutiny(e: Election[B], numVacancies: Int): List[(Candidate,Rational)] 
  def computeWinners(e: Election[B], numVacancies: Int): List[(Candidate,Rational)] 
  
  def computeQuota(numVotes: Int, numVacancies: Int): Rational
  def cutQuotaFraction(num: Rational): Rational 

  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] 
  
  def computeTransferValue(surplus: Rational, election: Election[B], pendingWinners:  List[Candidate], candidate: Candidate, markings: Option[Set[Int]]): Rational
 
  def distributeSurplusVotes(election: Election[B], candidate: Candidate, total:Rational, markings: Option[Set[Int]], pendingWinners: List[Candidate], transferValue: Rational): Election[B]
  
  def chooseCandidateForExclusion(totals: Map[Candidate, Rational]): (Candidate, Rational)
  
  def resolveExclusionTie(totals: Map[Candidate, Rational]): (Candidate, Rational)
  
  def exclude(election: Election[B], candidate: Candidate, value: Option[Rational], newWinners: Option[List[Candidate]]): Election[B]

 def printElection(election: Election[B]) = {
    print("\n")
    for (e <- election.sortBy(x => x.id)) {
      var pr = ""
      for (p <- e.preferences) pr = pr + p + " > "
      println(e.id + "   " + pr.dropRight(2) + "  " + e.weight)
    }
    print("\n")
 }
 
  
 def getCandidates(election: Election[B]): List[Candidate] = {
   var set = new HashSet[Candidate]()
   for (b <- election) {
     for (c <- b.preferences)
       if (!set.exists(n => n == c) )  set = set + c 
   }
   set.toList
 }
  
  
  def computeTotals(election: Election[WeightedBallot]): Map[Candidate, Rational] = {
      val m = new Map[Candidate, Rational]
    
      for (b <- election if !b.preferences.isEmpty) { 
        m(b.preferences.head) = b.weight + (m.getOrElse(b.preferences.head, 0))
      }
     m
  }

  def quotaReached(totals: Map[Candidate, Rational], quota: Rational): Boolean = {
     if (totals.exists(_._2 >= quota) ) {
       //println("\nQuota is reached")
       true
       }
     else {
       //println("\nQuota is not reached")
       false
     }
  }
  
  def vacanciesFilled(numWinners:Int, numVacancies:Int): Boolean = 
    numWinners >= numVacancies
    
    
  def ballotsAreContinuing(c: Candidate, election: Election[B], pendingWinners:  List[Candidate]): Boolean = {
    var el = election
    var ballotsC = false
    while (ballotsC==false && el.nonEmpty){
      val ballot = el.head 
      if (ballot.preferences.head == c && !ballot.preferences.tail.diff(pendingWinners).isEmpty )
        ballotsC = true
      el = el.tail
    }
    println("Has continuing candidates?: " + ballotsC)
    ballotsC
  }
    
    
  def removeWinnerFromElection(election: Election[B], winner: Candidate): Election[B] = {
   var list: Election[B] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head != winner) list = b::list     
   list
  }

  
  //TODO: Optimize: as soon as we found continuing candidate, we can simply attach the rest of the list
  def filterPreferences(preferences: List[Candidate], candidates: List[Candidate]): List[Candidate] = { 
   var newpreferences: List[Candidate] = Nil
   for (c <- preferences) {
     candidates.exists { x => x == c } match {
       case true => 
       case false => newpreferences = c::newpreferences 
     }
  }
  newpreferences.reverse
 }
 
  
 
  
}
  


