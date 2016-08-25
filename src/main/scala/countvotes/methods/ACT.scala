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



abstract class ACT extends STVMethod[ACTBallot]
 with DroopQuota
 with NoFractionInQuota
 with NewWinnersOrderedByTotals[ACTBallot]
 with ACTSurplusDistributionTieResolution
 with ACTFractionLoss
 with ACTExclusion
 with ACTExclusionTieResolution 
 with ACTExactWinnerRemoval
 {  
  
  def declareNewWinnersWhileExcluding(candidate: Candidate, exhaustedBallots: Set[ACTBallot], newtotals: Map[Candidate, Rational], totalsWithoutNewWinners: Map[Candidate, Rational], newElectionWithoutFractionInTotals: Election[ACTBallot]):  List[(Candidate,Rational)]
  
  def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)] 
  
  def rewriteTotalOfCandidate(totals: Map[Candidate, Rational], candidate: Candidate, newTotal: Option[Int]): Map[Candidate, Rational]
  
  def computeIncorrectTotalofEVACS(step: (Candidate, Rational), newElectionWithoutFractionInTotals: Election[ACTBallot]): Option[Int]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def filterBallotsWithFirstPreferences(election: Election[ACTBallot], preferences: List[Candidate]): Election[ACTBallot] = {
    var ballots:  Election[ACTBallot] = List()
    for (b <- election) {
      if (b.preferences.take(preferences.length) == preferences) ballots = b::ballots
    }
    ballots
  }
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def runScrutiny(election: Election[ACTBallot], numVacancies: Int):  Report[ACTBallot] = {  // all ballots of e are marked when the function is called
   val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
   println("Number of ballots:" + election.length)
   println("Quota: " + quota)
   result.setQuota(quota)
   report.setQuota(quota)
    
   val totals = computeTotals(election)
   result.addTotalsToHistory(totals) 
 
   report.setCandidates(getCandidates(election))
   report.newCount(Input, None, Some(election), Some(totals), None, None)
   report.setLossByFractionToZero
   
   report.setWinners(computeWinners(election, numVacancies))   
   
   report
 }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def computeWinners(election: Election[ACTBallot], numVacancies: Int): List[(Candidate,Rational)] = {
    
   println(" \n NEW RECURSIVE CALL \n")
           
   println(election)
   
   val ccands = getCandidates(election)
   println("Continuing candidates: " + ccands)
       
   val totals = computeTotals(election)  
   println("Totals: " + totals)
        
   //result.addTotalsToHistory(totals)
    
   // Notice: There may be more new winners than available vacancies!!! 
   // Apparently EVACS does not check this condition. See step 8.  Or in count.c
   // while (for_each_candidate(e->candidates, &check_status,
	//			  (void *)(CAND_PENDING|CAND_ELECTED))
	 //      != e->electorate->num_seats) {
   // That is why we also check only equality here
   if (ccands.length == numVacancies){
     var ws: List[(Candidate,Rational)] = List()
     for (c <- ccands) ws = (c, totals.getOrElse(c, Rational(0,1)))::ws
     report.newCount(VictoryWithoutQuota, None, None, None, Some(ws), None)
     report.setLossByFractionToZero
     for (c <- ccands) yield (c, totals.getOrElse(c, Rational(0,1)))
   }
   else {        
    quotaReached(totals, result.getQuota) match {
      case true => 
          val winners: List[(Candidate, Rational)] = returnNewWinners(totals, result.getQuota) // sorted!
          println("New winners: " + winners)
          result.addPendingWinners(winners.toList, Some(extractMarkings(election))) 
      
          vacanciesFilled(winners.length, numVacancies) match {
              case false =>  {
                println("Vacancies: not yet filled.")
                val res = surplusesDistribution(election,numVacancies-winners.length)
                val newElection: Election[ACTBallot] = res._1
                val newWinners: List[(Candidate, Rational)] = res._2
                
                val nws = winners.length + newWinners.length
                println("Number of winners in this recursive call: "  + nws)
                if (nws == numVacancies) { winners:::newWinners }
                else computeWinners(newElection, numVacancies-nws):::winners:::newWinners   // TODO: care should be taken that newElection is not empty?!
              }
              case true => winners
            }
          
      case false =>  
          val leastVotedCandidate = chooseCandidateForExclusion(totals)
          println("Candidate to be excluded: " + leastVotedCandidate )
          result.addExcludedCandidate(leastVotedCandidate._1,leastVotedCandidate._2)

          val res = exclusion(election, leastVotedCandidate._1, numVacancies)
          val newElection: Election[ACTBallot] = res._1
          val newWinners: List[(Candidate, Rational)] = res._2
          
          println("New winners: " + newWinners)
          println("Number of winners in this recursive call: "  + newWinners.length)
          if (newWinners.length == numVacancies) { 
            // Notice: There may be more new winners than available vacancies!!! 
            // Apparently EVACS does not check this condition. See step 42. Or in count.c
            // if (for_each_candidate(candidates, &check_status,(void *)(CAND_ELECTED|CAND_PENDING)) == num_seats) return true;
            newWinners }           
          else computeWinners(newElection, numVacancies-newWinners.length):::newWinners
          
      }
    
   }
  }
 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   def extractMarkings(election: Election[ACTBallot]): Set[Int] = {
     var markings: Set[Int]  = Set()
     for (b <- election){
       if (b.marking) {
         markings += b.id 
       }
     }
     markings
   }
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def surplusesDistribution(election: Election[ACTBallot], numVacancies: Int): (Election[ACTBallot], List[(Candidate,Rational)]) = {
  println("Distribution of surpluses.")
   var newws: List[(Candidate, Rational)] = List() 
   var newElection = election

   while (result.getPendingWinners.nonEmpty && newws.length != numVacancies){
    val (cand, ctotal, markings) = result.takeAndRemoveFirstPendingWinner

    val res = tryToDistributeSurplusVotes(newElection, cand, ctotal, markings)
    println("newElection" + newElection)
    newElection = res._1
    newws = newws ::: res._2
    println("Are there pending candidates? " + result.getPendingWinners.nonEmpty)
   }
   (newElection, newws)
  }
  
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def tryToDistributeSurplusVotes(election: Election[ACTBallot], winner: Candidate, ctotal:Rational, markings: Option[Set[Int]] ): (Election[ACTBallot], List[(Candidate,Rational)]) = {

  val pendingWinners = result.getPendingWinners.map(x => x._1)

  if (ctotal == result.getQuota || !ballotsAreContinuing(winner, election, pendingWinners) )  
   { 
    println("I AM HERE")
      val newElection = removeWinnerWithoutSurplusFromElection(election, winner) // should not we remove the candidate from all preferences in ballots???
      result.removePendingWinner(winner)
      println(newElection)
      (newElection, List())
   }
  else {
    println("Distributing the surplus of " + winner) 
    
    val surplus = ctotal - result.getQuota
    
    val tv = computeTransferValue(surplus, election, pendingWinners, winner, markings) 
    println("tv = " + tv)
        
    val (newElection, exhaustedBallots, ignoredBallots) = distributeSurplusVotes(election, winner, ctotal, markings, pendingWinners, tv)  
    val newElectionWithoutFractionInTotals = loseFraction(newElection)
           
    val newtotalsWithoutFraction = computeTotals(newElectionWithoutFractionInTotals)
    val newtotalsWithoutFractionWithoutpendingwinners = newtotalsWithoutFraction.clone().retain((k,v) => !pendingWinners.contains(k)) 
    
    
    result.removePendingWinner(winner)
    
    result.addTotalsToHistory(newtotalsWithoutFractionWithoutpendingwinners)
    var ws = declareNewWinnersWhileDistributingSurpluses(newtotalsWithoutFractionWithoutpendingwinners,newElection)

    //------------ Reporting ------------------------------------------
    if (ws.nonEmpty) report.newCount(SurplusDistribution, Some(winner), Some(newElectionWithoutFractionInTotals), Some(newtotalsWithoutFraction), Some(ws), Some(exhaustedBallots))
    else report.newCount(SurplusDistribution, Some(winner), Some(newElectionWithoutFractionInTotals), Some(newtotalsWithoutFraction), None, Some(exhaustedBallots))
    report.setLossByFraction(computeTotals(newElection), newtotalsWithoutFraction)
    ignoredBallots match { // ballots ignored because they don't belong to the last parcel of the winner
      case Some(ib) => report.setIgnoredBallots(ib)
      case None =>
    }
    //------------------------------------------------------------------

    (newElectionWithoutFractionInTotals, ws)
  }
 }
 
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def exclusion(election: Election[ACTBallot], candidate: Candidate, numVacancies: Int): (Election[ACTBallot],  List[(Candidate, Rational)] ) = { 
   println("Vacancies left: " + numVacancies)
   

   var ws: List[(Candidate,Rational)] = List()
   var newws: List[(Candidate,Rational)] = List()
   var steps = determineStepsOfExclusion(election,candidate)
   var newElection = election
   var newElectionWithoutFractionInTotals = election
   var exhaustedBallots: Set[ACTBallot] = Set()
   while (ws.length != numVacancies && !steps.isEmpty){
    val step = steps.head
    println("Step of exclusion: " + step)
    steps = steps.tail // any better way to do this?
    
    val newTotal = computeIncorrectTotalofEVACS(step, newElectionWithoutFractionInTotals) // simulating EVACS's incorrect total as a result of partial exclusion
    
    val ex = exclude(newElectionWithoutFractionInTotals, step._1, Some(step._2), Some(newws.map(x => x._1)))

    newElection = ex._1
    
    exhaustedBallots = ex._2

    val totalsBeforeFractionLoss = computeTotals(newElection) // for computing LbF

    newElectionWithoutFractionInTotals = loseFraction(newElection) // perhaps it is better  to get rid of newws in a separate function

    val totalsAfterFractionLoss = computeTotals(newElectionWithoutFractionInTotals)
    
    val totalsWithIncorrectValueForCandidate = rewriteTotalOfCandidate(totalsAfterFractionLoss, candidate, newTotal) // simulating EVACS's incorrect total as a result of partial exclusion
    
    val totalsWithoutNewWinners = totalsWithIncorrectValueForCandidate.clone().retain((k,v) => !ws.map(_._1).contains(k)) // excluding winners that are already identified in the while-loop
    result.addTotalsToHistory(totalsWithIncorrectValueForCandidate)
    println("Totals: " + totalsWithIncorrectValueForCandidate)
    
    newws = declareNewWinnersWhileExcluding(candidate, exhaustedBallots, totalsWithIncorrectValueForCandidate,totalsWithoutNewWinners, newElectionWithoutFractionInTotals)
    
    ws = ws ::: newws 
    
    
    report.setLossByFraction(totalsBeforeFractionLoss, totalsWithIncorrectValueForCandidate)
    //report.setIgnoredBallots(List())
   }
  // TODO  distribute remaining votes
  // if (vacanciesFilled(ws.length, numVacancies)) { 
  // }
   var dws:  List[(Candidate, Rational)]  = List()
   if (ws.nonEmpty) {
     val res = surplusesDistribution(newElectionWithoutFractionInTotals, numVacancies - ws.length)
     println("HERE " + res._1)
     newElectionWithoutFractionInTotals = res._1
     dws = res._2
   }
   
   (newElectionWithoutFractionInTotals, ws:::dws)
 }
  
 
 
// ACT Legislation:
// 9(1): If a candidate is excluded in accordance with clause 8, the ballot papers counted for the candidate 
// shall be sorted into groups according to their transfer values when counted for him or her. 
//
 def determineStepsOfExclusion(election: Election[ACTBallot], candidate: Candidate): List[(Candidate, Rational)] = {
   var s: Set[(Candidate, Rational)] = Set()

   for (b <- election) { 
      if (b.preferences.nonEmpty && b.preferences.head == candidate && !s.contains((candidate,b.value))) { 
        s += ((candidate, b.value)) }
    }
   s.toList.sortBy(x => x._2).reverse //>
 }

  
  
  
}