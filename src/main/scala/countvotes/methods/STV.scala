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


abstract class STV[B <: Ballot with Weight] extends VoteCountingMethod[B] {
  //type E = Election[B]

  // GLOBAL MUTABLE VARIABLES
  // DON'T FORGET TO RESET
  protected val result: Result = new Result
  protected val report: Report[B] = new Report[B]
  
  def runScrutiny(e: Election[B], ccandidates: List[Candidate], numVacancies: Int):   Report[B]
  
  def computeQuota(numVotes: Int, numVacancies: Int): Rational
  
  def cutQuotaFraction(num: Rational): Rational

  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)]

  def computeTransferValue(surplus: Rational, election: Election[B], pendingWinners:  List[Candidate], candidate: Candidate, markings: Option[Set[Int]]): Rational
   
  def distributeSurplusVotes(election: Election[B],  candidate: Candidate, total:Rational, markings: Option[Set[Int]], pendingWinners: List[Candidate], transferValue: Rational): (Election[B], Set[B], Option[Election[B]])
  
  def resolveSurpluseDistributionTie(equaltotals: Map[Candidate, Rational]): List[(Candidate, Rational)]

  def chooseCandidateForExclusion(totals: Map[Candidate, Rational]): (Candidate, Rational)

  def exclude(election: Election[B], candidate: Candidate, value: Option[Rational], newWinners: Option[List[Candidate]]): (Election[B], Set[B])

  def removeWinnerWithoutSurplusFromElection(election: Election[B], winner: Candidate): Election[B]

  
  //def run(e: Election[B], numVacancies: Int):   Report[B] = {
  //  val output = runScrutiny(e: Election[B], numVacancies: Int)
  //  result.clear
  //  report.clear
  //  output
  //}
  
  
  def computeTotals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
       
     val m = new Map[Candidate, Rational]
    
      for (c<-candidates) m(c) = 0
      
      for (b <- election if !b.preferences.isEmpty) {
        m(b.preferences.head) = b.weight + (m.getOrElse(b.preferences.head, 0))
      }         
     m
  }

  def sumTotals(totals:  Map[Candidate, Rational]): Rational = {
    var sum: Rational = 0
    for (t <- totals) {
      sum += t._2
    } 
    sum
  }

  def computeTotal(election: Election[WeightedBallot], candidate: Candidate): Rational = {
     var r: Rational = 0
     for (b <- election if (!b.preferences.isEmpty && b.preferences.head == candidate) )
       r = r + b.weight
     r
  }

  def quotaReached(totals: Map[Candidate, Rational], quota: Rational): Boolean = {
     if (totals.exists(_._2 >= quota) ) {
       println("\nQuota: reached")
       true
       }
     else {
       println("\nQuota: not reached")
       false
     }
  }

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
  


