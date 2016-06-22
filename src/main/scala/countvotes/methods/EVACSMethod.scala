package countvotes.methods

import countvotes.structures._
import countvotes.algorithms._

object EVACSMethod extends GenericSTVMethod[ACTBallot] 
 with DroopQuota
 with NoFractionInQuota
 with NewWinnersOrderedByTotals[ACTBallot]
 with TransferValueWithDenominatorWithNumOfMarkedContinuingBallots
 with ACTSurplusDistribution
 with ACTFractionLoss
 with ACTCandidateForExclusion
 with ACTExclusion
 with UnfairExclusionTieResolutuim // <- TODO !
 {  
  
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def runScrutiny(election: Election[ACTBallot], numVacancies: Int): List[(Candidate,Rational)]  = {  // all ballots of e are marked when the function is called
  
   val quota = cutQuotaFraction(computeQuota(election.length, numVacancies))
   println("Quota = " + quota)
   result.setQuota(quota)
         
   computeWinners(election, numVacancies)   
 }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def computeWinners(election: Election[ACTBallot], numVacancies: Int): List[(Candidate,Rational)] = {
    
    println(" \n NEW RECURSIVE CALL \n")
    
    //println("Election: " + election)
    
    val ccands = getCandidates(election)
       
    val totals = computeTotals(election)  
    println("Totals: " + totals)
    
   if (ccands.length <= numVacancies){
     for (c <- ccands) yield (c, totals(c))
   }
   else {  
    quotaReached(totals, result.getQuota) match {
      case true => 
          println("The quota is reached.")
          val winners: List[(Candidate, Rational)] = returnNewWinners(totals, result.getQuota) // sorted!
          println("New winners: " + winners)
          result.addPendingWinners(winners.toList, extractMarkings(election)) 
      
          vacanciesFilled(winners.length, numVacancies) match {
              case false =>  {
                println("Vacancies are not yet filled.")
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
          println("Excluding " + leastVotedCandidate )
          result.addExcludedCandidate(leastVotedCandidate._1,leastVotedCandidate._2)
          val res = exclusion(election, leastVotedCandidate._1, numVacancies)
          val newElection: Election[ACTBallot] = res._1
          val newWinners: List[(Candidate, Rational)] = res._2
          
          println("new Winners " + newWinners)
          println("Number of winners in this recursive call: "  + newWinners.length)
          if (newWinners.length == numVacancies) { newWinners }
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
    val (cand, ctotal, markings) = result.takeFirstPendingWinner
    
    val res = tryToDistributeSurplusVotes(newElection, cand, ctotal, Some(markings))
    newElection = res._1
    newws = newws ::: res._2
    println("res._2 = " + res._2)
    println("Are there pending candidates? " + result.getPendingWinners.nonEmpty)
   }
   (newElection, newws)
  }
  
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def tryToDistributeSurplusVotes(election: Election[ACTBallot], winner: Candidate, ctotal:Rational, markings: Option[Set[Int]] ): (Election[ACTBallot], List[(Candidate,Rational)]) = {
 
  val pendingWinners = result.getPendingWinners.map(x => x._1)
  
  if (ctotal == result.getQuota || !ballotsAreContinuing(winner, election, pendingWinners) )  
   { 
      val newElection = removeWinnerFromElection(election, winner) // should not we remove the candidate from all preferences in ballots???
      result.removePendingWinner(winner)
      (newElection, List())
   }
  else {
    println("Distributing the surplus of " + winner) 
    
    val surplus = ctotal - result.getQuota
    
    val tv = computeTransferValue(surplus, election, pendingWinners, winner, markings) 
    println("tv = " + tv)
        
    val newElection = distributeSurplusVotes(election, winner, ctotal, markings, pendingWinners, tv)    
    val newElectionWithoutFractionInTotals = loseFraction(newElection)
    
    result.removePendingWinner(winner)
           
    val newtotals = computeTotals(newElectionWithoutFractionInTotals).clone().retain((k,v) => !pendingWinners.contains(k)) // excluding pending winners
    var ws:  List[(Candidate,Rational)] = List()
    if (quotaReached(newtotals, result.getQuota)){
     ws = returnNewWinners(newtotals, result.getQuota) // sorted!
     result.addPendingWinners(ws.toList, extractMarkings(newElection)) 
    }
    (newElectionWithoutFractionInTotals, ws)
  }
 }
 
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def exclusion(election: Election[ACTBallot], candidate: Candidate, numVacancies: Int): (Election[ACTBallot],  List[(Candidate, Rational)] ) = { 
   println("Exclusion of " + candidate)
   println("Vacancies left: " + numVacancies)
   var ws: List[(Candidate,Rational)] = List()
   var newws: List[(Candidate,Rational)] = List()
   var steps = determineStepsOfExclusion(election,candidate)
   var newElection = election
   while (ws.length != numVacancies && !steps.isEmpty){
    val step = steps.head
    println("Step of exclusion " + step)
    steps = steps.tail // any better way to do this?
    newElection = loseFraction(exclude(newElection, step._1, step._2, newws.map(x => x._1))) // perhaps it is better  to get rid of newws in a separate function
    val totals = computeTotals(newElection).clone().retain((k,v) => !ws.map(_._1).contains(k)) // excluding winners that are already identified in the while-loop
    println("totals " + totals)
    if (quotaReached(totals, result.getQuota) ) {
      newws = returnNewWinners(totals, result.getQuota) // sorted!
      println("New winners as a result of the current partial exclusion: " + newws)
      result.addPendingWinners(newws.toList, extractMarkings(newElection)) 
      ws = ws ::: newws // check that the order is correct here!!!
    }
   }
  // TODO  distribute remaining votes
  // if (vacanciesFilled(ws.length, numVacancies)) { 
  // }
   var dws:  List[(Candidate, Rational)]  = List()
   if (ws.nonEmpty) {
     val res = surplusesDistribution(newElection, numVacancies - ws.length)
     newElection = res._1
     dws = res._2
   }
   
   (newElection, ws:::dws)
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
