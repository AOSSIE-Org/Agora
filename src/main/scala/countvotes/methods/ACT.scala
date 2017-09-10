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


abstract class ACT extends STVAustralia
 with DroopQuota
 with NoFractionInQuota
 with NewWinnersOrderedByTotals[ACTBallot]
 with ACTSurplusDistributionTieResolution
 with ACTFractionLoss
 with ACTExclusion
 with ACTExclusionTieResolution
 with ACTExactWinnerRemoval
 {
  
//  val result: Result = new Result
//  val report: Report[ACTBallot] = new Report[ACTBallot]

  def declareNewWinnersWhileExcluding(
    candidate: Candidate, exhaustedBallots: Set[ACTBallot], newtotals: Map[Candidate, Rational],
    totalsWithoutNewWinners: Map[Candidate, Rational], newElectionWithoutFractionInTotals: Election[ACTBallot]):
  List[(Candidate,Rational)]

  def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)]

  def rewriteTotalOfCandidate(totals: Map[Candidate, Rational], candidate: Candidate, newTotal: Option[Int]): Map[Candidate, Rational]

  def computeIncorrectTotalofEVACS(step: (Candidate, Rational), newElectionWithoutFractionInTotals: Election[ACTBallot]): Option[Int]

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def filterBallotsWithFirstPreferences(election: Election[ACTBallot], preferences: List[Candidate]): Election[ACTBallot] = {
    var ballots:  List[ACTBallot] = List()
    for (b <- election) {
      if (b.preferences.take(preferences.length) == preferences) ballots = b::ballots
    }
    Election(ballots)
  }


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def winners(election: Election[ACTBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {

   println(" \n NEW RECURSIVE CALL \n")

   //println("Election: " + election)

   if (election.isEmpty){Nil}  // If all ballots are removed by the candidate who reached the quota exactly, the election will be empty.
   //                             For example (3 seats, quota=2):
   //                              1 1/1 2
   //                              2 1/1 2
   //                              3 1/1 2
   //                              4 1/1 5>6>1
   //                              5 1/1 5>3>6
   else {

   //val ccands = getCandidates(election)
   println("Continuing candidates: " + ccandidates)

   val tls = Election.totals(election, ccandidates)
   println("Totals: " + tls)

   //result.addTotalsToHistory(totals)

    // Notice: There may be more new winners than available vacancies!!!
    // Apparently EVACS does not check this condition. See step 8.  Or in count.c
   // while (for_each_candidate(e->candidates, &check_status,
  //        (void *)(CAND_PENDING|CAND_ELECTED))
   //      != e->electorate->num_seats) {
   // That is why we also check only equality here
   if (ccandidates.length == numVacancies){
     var ws: List[(Candidate,Rational)] = List()
     for (c <- ccandidates) ws = (c, tls.getOrElse(c, Rational(0,1)))::ws
     report.newCount(VictoryWithoutQuota, None, None, None, Some(ws), None)
     report.setLossByFractionToZero
     for (c <- ccandidates) yield (c, tls.getOrElse(c, Rational(0,1)))
   }
   else {
    quotaReached(tls, result.getQuota) match {
      case true => {
          val ws: List[(Candidate, Rational)] = returnNewWinners(tls, result.getQuota) //  sorted! tie resolved!
          println("New winners: " + ws)
          result.addPendingWinners(ws.toList, Some(extractMarkings(election)))

          val vacanciesFilled = ws.length >= numVacancies
          
          vacanciesFilled match {
              case false =>  {
                println("Vacancies: not yet filled.")
                val res = surplusesDistribution(election, ccandidates, numVacancies-ws.length)
                val newElection: Election[ACTBallot] = res._1
                val newWinners: List[(Candidate, Rational)] = res._2

                val nws = ws.length + newWinners.length
                println("Number of winners in this recursive call: "  + nws)
                val allWinners = ws:::newWinners
                if (nws == numVacancies) { allWinners }
                else {
                  val setAllWinners = allWinners.map{_._1}.toSet
                  winners(newElection, ccandidates.filterNot(setAllWinners.contains(_)) ,numVacancies-nws):::allWinners
                  // TODO: care should be taken that newElection is not empty?!
                }
                }
              case true => ws
            }
      }
      case false => {
          val leastVotedCandidate = chooseCandidateForExclusion(tls)
          println("Candidate to be excluded: " + leastVotedCandidate )
          result.addExcludedCandidate(leastVotedCandidate._1,leastVotedCandidate._2)

          val res = exclusion(election, ccandidates, leastVotedCandidate, numVacancies)
          val newElection: Election[ACTBallot] = res._1
          val newWinners: List[(Candidate, Rational)] = res._2

          println("New winners: " + newWinners)
          println("Number of winners in this recursive call: "  + newWinners.length)
          if (newWinners.length == numVacancies) {
            // Notice: There may be more new winners than available vacancies!!!
            // Apparently EVACS does not check this condition. See step 42. Or in count.c
            // if (for_each_candidate(candidates, &check_status,(void *)(CAND_ELECTED|CAND_PENDING)) == num_seats) return true;
            newWinners
          }
          else {
            winners(newElection,ccandidates.filterNot(x => x == leastVotedCandidate._1), numVacancies-newWinners.length):::newWinners
          }
      }
      }

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

  def surplusesDistribution(
    election: Election[ACTBallot], ccandidates: List[Candidate], numVacancies: Int):
  (Election[ACTBallot], List[(Candidate,Rational)]) = {
  println("Distribution of surpluses.")
   var newws: List[(Candidate, Rational)] = List()
   var newElection = election

   while (result.getPendingWinners.nonEmpty && newws.length != numVacancies){
    val (cand, ctotal, markings) = result.takeAndRemoveFirstPendingWinner
    val res = tryToDistributeSurplusVotes(newElection, ccandidates, cand, ctotal, markings)
    newElection = res._1
    newws = newws ::: res._2
    println("Are there pending candidates? " + result.getPendingWinners.nonEmpty)
   }
   (newElection, newws)
  }

 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def tryToDistributeSurplusVotes(
   election: Election[ACTBallot], ccandidates: List[Candidate], winner: Candidate,
   ctotal:Rational, markings: Option[Set[Int]] ):
 (Election[ACTBallot], List[(Candidate,Rational)]) = {

  val pendingWinners = result.getPendingWinners.map(x => x._1)

  if (ctotal == result.getQuota) {
      val newElection = removeWinnerWithoutSurplusFromElection(election, winner)
      result.removePendingWinner(winner)
      (newElection, List())
   }
  else
    // NOTE THAT WHEN (!ballotsAreContinuing(winner, election, pendingWinners))  THE ELECTION DOES NOT CHANGE
    //
    //  if (!ballotsAreContinuing(winner, election, pendingWinners) ) {
    //    val newElection = ???
    //    result.removePendingWinner(winner)
    //    (newElection, List())
    //  }
    //  else
    {
    println("Distributing the surplus of " + winner)

    val surplus = ctotal - result.getQuota

    val tv = computeTransferValue(surplus, election, pendingWinners, winner, markings)
    println("tv = " + tv)

    val (newElection, exhaustedBallots, ignoredBallots) = distributeSurplusVotes(election, winner, ctotal, markings, pendingWinners, tv)
    val newElectionWithoutFractionInTotals = loseFraction(newElection, ccandidates)

    val newtotalsWithoutFraction = Election.totals(newElectionWithoutFractionInTotals, ccandidates)
    val newtotalsWithoutFractionWithoutpendingwinners = newtotalsWithoutFraction.clone().retain((k,v) => !pendingWinners.contains(k))


    result.removePendingWinner(winner)

    result.addTotalsToHistory(newtotalsWithoutFractionWithoutpendingwinners)
    var ws = declareNewWinnersWhileDistributingSurpluses(newtotalsWithoutFractionWithoutpendingwinners,newElection)

    //------------ Reporting ------------------------------------------
    if (ws.nonEmpty) {
      report.newCount(
        SurplusDistribution, Some(winner), Some(newElectionWithoutFractionInTotals),
        Some(newtotalsWithoutFraction), Some(ws), Some(exhaustedBallots))
    }
    else {
      report.newCount(
        SurplusDistribution, Some(winner), Some(newElectionWithoutFractionInTotals),
        Some(newtotalsWithoutFraction), None, Some(exhaustedBallots))
    }
    report.setLossByFraction(Election.totals(newElection,ccandidates), newtotalsWithoutFraction)
    ignoredBallots match { // ballots ignored because they don't belong to the last parcel of the winner
      case Some(ib) => report.setIgnoredBallots(ib)
      case None =>
    }
    //------------------------------------------------------------------

    (newElectionWithoutFractionInTotals, ws)
  }
 }

 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def exclusion(
   election: Election[ACTBallot], ccandidates: List[Candidate],
   candidate: (Candidate, Rational), numVacancies: Int):
 (Election[ACTBallot],  List[(Candidate, Rational)] ) = {
   println("Vacancies left: " + numVacancies)


   var ws: List[(Candidate,Rational)] = List()
   var newws: List[(Candidate,Rational)] = List()
   var newElection = election
   var newElectionWithoutFractionInTotals = election
   var exhaustedBallots: Set[ACTBallot] = Set()

  if (candidate._2 == Rational(0,1)){
    println("Excluding candidate with zero votes: " + candidate)

    val ex = excludeZero(election, candidate._1)

    (ex._1,ws)
  }
  else {
   var steps = determineStepsOfExclusion(election,candidate._1)

   while (ws.length != numVacancies && !steps.isEmpty){
    val step = steps.head
    println("Step of exclusion: " + step)
    steps = steps.tail // any better way to do this?

    val newTotal = computeIncorrectTotalofEVACS(step, newElectionWithoutFractionInTotals) // simulating EVACS's incorrect total as a result of partial exclusion

    val ex = exclude(newElectionWithoutFractionInTotals, step._1, Some(step._2), Some(newws.map(x => x._1)))

    newElection = ex._1

    exhaustedBallots = ex._2

    val totalsBeforeFractionLoss = Election.totals(newElection, ccandidates) // for computing LbF

    newElectionWithoutFractionInTotals = loseFraction(newElection, ccandidates) // perhaps it is better  to get rid of newws in a separate function

    val totalsAfterFractionLoss = Election.totals(newElectionWithoutFractionInTotals, ccandidates)

    val totalsWithIncorrectValueForCandidate = rewriteTotalOfCandidate(totalsAfterFractionLoss, candidate._1, newTotal)
    // simulating EVACS's incorrect total as a result of partial exclusion

    val totalsWithoutNewWinners = totalsWithIncorrectValueForCandidate.clone().retain((k,v) => !ws.map(_._1).contains(k))
    // excluding winners that are already identified in the while-loop

    result.addTotalsToHistory(totalsWithIncorrectValueForCandidate)
    println("Totals: " + totalsWithIncorrectValueForCandidate)

    newws =
      declareNewWinnersWhileExcluding(
        candidate._1, exhaustedBallots, totalsWithIncorrectValueForCandidate,totalsWithoutNewWinners,
        newElectionWithoutFractionInTotals)

    ws = ws ::: newws


    report.setLossByFraction(totalsBeforeFractionLoss, totalsWithIncorrectValueForCandidate)
    //report.setIgnoredBallots(List())
   }
  // TODO  distribute remaining votes
  // if (vacanciesFilled(ws.length, numVacancies)) {
  // }
   var dws:  List[(Candidate, Rational)]  = List()
   if (ws.nonEmpty) {
     val res = surplusesDistribution(newElectionWithoutFractionInTotals, ccandidates.filterNot { x => x == candidate }, numVacancies - ws.length)
     newElectionWithoutFractionInTotals = res._1
     dws = res._2
   }

   (newElectionWithoutFractionInTotals, ws:::dws)
  }
 }




}
