package agora.votecounter

import agora.model._
import agora.votecounter.stv._

import spire.math.Rational

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => MMap}
import collection.Map
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._
import agora.votecounter.stv.VictoryWithoutQuota
import agora.votecounter.stv.TwoLastCandidatesForOneVacancy
import agora.votecounter.stv.SurplusDistribution
import agora.votecounter.stv.ACTBallot

abstract sealed class BulkExclusionType
  case object ExclusionBulk extends BulkExclusionType
  case object SurplusDistributionBulk extends BulkExclusionType

class AustralianSenate extends STVAustralia
 with DroopQuota // Section 273 (8)
 with NoFractionInQuota // Section 273 (8)
 with NewWinnersOrderedByTotals[ACTBallot] // TODO
 with SenateSurplusDistributionTieResolution // Section 273 (22)
 with ACTFractionLoss //
 with SenateExclusion //   exactly like ACTExclusion
 with SenateExactWinnerRemoval // exactly like ACTExactWinnerRemoval
 with TransferValueWithDenominatorWithNumOfBallots // Section 273 (9)(a)
 with SenateSurplusDistribution // Section 273 (9)(b)
 with SenateNewWinnersDuringSurplusesDistribution
 with SenateNewWinnersDuringExclusion
 with UnfairExclusionTieResolution // TODO
 {


  //def declareNewWinnersWhileDistributingSurpluses(totals: Map[Candidate, Rational], election:Election[ACTBallot]):  List[(Candidate,Rational)]


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//       Functions for  Bulk Exclusion - Section 273 (13A), (13B), (13C)
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  // Section 273 (29)
  def computeNotionalVotes(candidate: Candidate, totals: Map[Candidate, Rational]): Rational = {
    totals.filter(p => p._2 < totals(candidate)).foldLeft(Rational(0,1))(_ + _._2)
  }

  def computeAdjustedNotionalVotes(candidate: Candidate, totals: Map[Candidate, Rational], surplus: Option[Rational]): Rational = {
      surplus match {
               case Some(s) =>  computeNotionalVotes(candidate: Candidate, totals: Map[Candidate, Rational] ) + s
               case None => throw new Exception("Surplus is None in computeAdjustedNotionalVotes.")
      }
  }

  def computeShortfall(candidate: Candidate, totals: Map[Candidate, Rational], quota: Rational): Rational = {
    quota - totals(candidate)
  }

  def returnLeadingShortfall(totals: Map[Candidate, Rational], quota: Rational): Rational = {
    quota - totals.valuesIterator.max
  }

  def computeVacancyShortfall(totals: Map[Candidate, Rational], numRemainingVacancies: Int,  quota: Rational): Rational = {
    val orderedTotals = totals.toList.sortBy(_._2).reverse
    // it does not matter how tie of equal values is resolved here, because we care only about values, hence - simple sort
   // orderedTotals.take(numRemainingVacancies).foldLeft(Rational(0,1))(_+(quota-totals(_._2)))
   var aggregate = Rational(0,1)
   for (candidate <- orderedTotals.take(numRemainingVacancies)) aggregate += (quota - totals(candidate._1))
   aggregate
  }

 def returnCandidateA(
   totals: Map[Candidate, Rational], vacancyShortfall: Rational,
   bulktype: BulkExclusionType, surplus: Option[Rational]):
 Option[Candidate] ={

   var pickedTotals: Map[Candidate, Rational] = Map()
   bulktype match {
          case ExclusionBulk => pickedTotals = totals.filter(p => computeNotionalVotes(p._1,totals) >= vacancyShortfall)   // Section 273 (13A)(a)
          case SurplusDistributionBulk =>
                pickedTotals = totals.filter(p => computeAdjustedNotionalVotes(p._1,totals, surplus) >= vacancyShortfall)   // Section 273 (13A)(a)
   }
   if (pickedTotals.nonEmpty){
    pickedTotals.filter(p => p._2 == pickedTotals.valuesIterator.min)
    if (pickedTotals.size != 1){
      // not specified in the legislation how this case should be addressed.
      // Section 273 (13A)(a) says "stands lower or lowest in the poll"
      println("More than one candidate satisfy conditions of Candidate A: Section 273 (13A)(a). One of them is picked: " + pickedTotals.head._1)
      Some(pickedTotals.head._1)
    }
    else {
      Some(pickedTotals.head._1)
    }
   }
   else {
     None // CandidateA is unidentified
   }
 }

 def returnCandidateB(
   totals: Map[Candidate, Rational], candidateA: Option[Candidate],
   vacancyShortfall: Rational, bulktype: BulkExclusionType, surplus: Option[Rational]):
 Option[Candidate] = {

   var totalsOfCandidatesPotentiallyB:  Map[Candidate, Rational] = Map()
   candidateA match {
    case Some(cA) => {
      totalsOfCandidatesPotentiallyB = totals.filter(p => p._2 < totals(cA))
     // val candidateB = totals.clone().filter(p => p._2 == valueOfCandidateB).head._1
    }
    case None => {
       bulktype match {
          case ExclusionBulk => totalsOfCandidatesPotentiallyB = totals.filter(p => computeNotionalVotes(p._1, totals) < vacancyShortfall)
          case SurplusDistributionBulk =>  totalsOfCandidatesPotentiallyB =
            totals.filter(p => computeAdjustedNotionalVotes(p._1, totals, surplus) < vacancyShortfall)
       }
      //val candidateB = totals.clone().filter(p => p._2 == valueOfCandidateB).head._1
    }
  }
  if  (totalsOfCandidatesPotentiallyB.nonEmpty) {
    val orderedTotalsOfCandidatesPotentiallyB = totalsOfCandidatesPotentiallyB.toList.sortBy(_._2) //TODO: sort appropriately
    val candidatesB = for (
      (left,right) <- (orderedTotalsOfCandidatesPotentiallyB zip orderedTotalsOfCandidatesPotentiallyB.tail)
      if (computeNotionalVotes(left._1, totals) < right._2)) yield left
    if (candidatesB.nonEmpty) Some(candidatesB.head._1) else None  // TODO: tail?
  }
  else {
    None
  }

 }


 def returnCandidateC(
   totals: Map[Candidate, Rational], leadingShortFall: Rational,
   bulktype: BulkExclusionType, surplus: Option[Rational]):
 Option[Candidate] = {

   var potentialCandidatesC: Map[Candidate, Rational] = Map()
   bulktype match {
          case ExclusionBulk => potentialCandidatesC = totals.filter(p => computeNotionalVotes(p._1, totals)<leadingShortFall)
          case SurplusDistributionBulk =>  potentialCandidatesC = totals.filter(
            p => computeAdjustedNotionalVotes(p._1, totals, surplus)<leadingShortFall)
   }
  println("potentialCandidatesC " + potentialCandidatesC)
  if (potentialCandidatesC.nonEmpty) {
    Some(potentialCandidatesC.toList.sortBy(_._2).head._1) //TODO: sort appropriately
  }
  else {
    None
  }
 }


 def selectCandidatesForBulkExclusion(
   totals: Map[Candidate, Rational], numRemainingVacancies: Int, quota: Rational,
   bulktype: BulkExclusionType, surplus: Option[Rational]):
    List[(Candidate, Rational)] = {

   val orderedCandidates = totals.toList.sortBy(_._2) //TODO: sort appropriately
   println("orderedCandidates: " + orderedCandidates)
   val vacancyShortfall = computeVacancyShortfall(totals, numRemainingVacancies, quota)
   println("vacancyShortfall: " + vacancyShortfall)
   var candidateB: Option[Candidate] = None
   bulktype match {
          case ExclusionBulk => {
            val candidateA = returnCandidateA(totals, vacancyShortfall, ExclusionBulk, None)
            println("candidateA: " + candidateA)
            candidateB = returnCandidateB(totals, candidateA, vacancyShortfall, ExclusionBulk, None)
            println("candidateB: " + candidateB)
          }
          case SurplusDistributionBulk => {
             val candidateA = returnCandidateA(totals, vacancyShortfall, SurplusDistributionBulk, surplus)
             println("candidateA: " + candidateA)
             candidateB = returnCandidateB(totals, candidateA, vacancyShortfall, SurplusDistributionBulk, surplus)
             println("candidateB: " + candidateB)
          }
   }
   candidateB match {
     case Some(cB) => {  // "in a case where Candidate B has been identified"
        var notionalVotesOfB: Rational = Rational(0,1)
        bulktype match {
          case ExclusionBulk => notionalVotesOfB = computeNotionalVotes(cB, totals)
          case SurplusDistributionBulk =>  notionalVotesOfB = computeAdjustedNotionalVotes(cB, totals, surplus)
        }
        val leadingShortfall = returnLeadingShortfall(totals, quota)
        if (notionalVotesOfB < leadingShortfall){ // Section273 (13A)(c)
          orderedCandidates.take(orderedCandidates.indexOf(cB) + 1)
        }
        else { // Section273 (13A)(d)
          var candidateC: Option[Candidate] = None
          bulktype match {
            case ExclusionBulk =>
              candidateC = returnCandidateC(totals, leadingShortfall, ExclusionBulk, None)
            case SurplusDistributionBulk =>
              candidateC = returnCandidateC(totals, leadingShortfall, SurplusDistributionBulk, surplus)
          }
          candidateC match {
            case Some(cC) => orderedCandidates.take(orderedCandidates.indexOf(candidateC) + 1)
            case None => List()
              //throw new Exception(
              //"CandidateC is None in selectCandidatesForBulkExclusion.
              //Is this case allowed/possible meaning that we can continue the scrutiny?
              //This is unclear from 13(A)(d). But can be analogously with 13(A)(b),(c) and (d) for CandidateB.")
          }
        }
     }
     case None => List()  // Candidate B has not been identified
   }
 }
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// End of functions for Bulk Exclusion
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  def filterBallotsWithFirstPreferences(election: Election[ACTBallot], preferences: List[Candidate]): Election[ACTBallot] = {
    var ballots:  List[ACTBallot] = List()
    for (b <- election) {
      if (b.preferences.take(preferences.length) == preferences) ballots = b::ballots
    }
    Election(ballots)
  }


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Differs from ACTMethod as follows:
// - absence of ``Last Parcel'', hence None in place of markings
// - existence of ``Bulk exclusion''
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  def winners(election: Election[ACTBallot], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate,Rational)] = {

   println(" \n NEW RECURSIVE CALL \n")

   ////printElection(election)

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

   val tls = election.firstVotes(ccandidates)

   //result.addTotalsToHistory(tls)

   // TODO: Section 273(17) (when only two continuing candidates remain for a single seat)
   // Notice: There may be more new winners than available vacancies!!!
   if (ccandidates.length == numVacancies){ // Section 273(18)
     println("HERE")
     val ws = for (c <- ccandidates) yield (c, tls.getOrElse(c, Rational(0,1)))
     report.newCount(VictoryWithoutQuota, None, None, None, Some(ws), None)
     report.setLossByFractionToZero
     for (c <- ccandidates) yield (c, tls.getOrElse(c, Rational(0,1)))
   }
   else {
   if (numVacancies==1 && ccandidates.length==2) {
     println(TwoLastCandidatesForOneVacancy)
     var ws: List[(Candidate, Rational)] = List()
     val c1 = ccandidates(0)
     val c2 = ccandidates(1)
     if (tls(c1)>tls(c2)) {
        ws = (c1, tls.getOrElse(c1, Rational(0,1)))::ws
     }
     else {
      if (tls(c1)<tls(c2)) {
        ws = (c2, tls.getOrElse(c2, Rational(0,1)))::ws
      }
      else {
       println("Tie has to be resolved!")
       ws = (c1, tls.getOrElse(c1, Rational(0,1)))::ws
      }
     }
     report.newCount(TwoLastCandidatesForOneVacancy, None, None, None, Some(ws), None)
     report.setLossByFractionToZero
     ws
   }
   else {
    quotaReached(tls, result.getQuota) match {
      case true => {
          val ws: List[(Candidate, Rational)] = returnNewWinners(tls, result.getQuota) // sorted! tie resolved!
          println("New winners: " + ws)
          result.addPendingWinners(ws.toList, None)

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
                  winners(newElection, ccandidates.filterNot(allWinners.map{_._1}.toSet.contains(_)) ,numVacancies-nws):::allWinners
                  // TODO: care should be taken that newElection is not empty?!
                }
                }
              case true => ws
            }
      }
      case false =>  {
        // Section 273 (13)(b) => (13A) and (13)(a) => (13AA)
         val candidatesToExclude = getCandidatesToExclude(tls, numVacancies, result.getQuota, ExclusionBulk, None)
         val res = exclusion(election, ccandidates, candidatesToExclude, numVacancies)
         val newElection: Election[ACTBallot]  = res._1
         val newWinners: List[(Candidate, Rational)] = res._2
         println("New winners: " + newWinners)
         println("Number of winners in this recursive call: "  + newWinners.length)
         if (newWinners.length == numVacancies) {
            // Notice: There may be more new winners than available vacancies!!!
            // if (for_each_candidate(candidates, &check_status,(void *)(CAND_ELECTED|CAND_PENDING)) == num_seats) return true;
            newWinners }
         else {
           winners(newElection,ccandidates.filterNot(x => candidatesToExclude.map(_._1).contains(x)), numVacancies-newWinners.length):::newWinners
         }
      }
      }

   }
   }
   }
  }

 def getCandidatesToExclude(
   totals: Map[Candidate, Rational], numRemainingVacancies: Int, quota: Rational,
   bulktype: BulkExclusionType, surplus: Option[Rational]):
    List[(Candidate, Rational)]  = {

   var candidatesToExclude:  List[(Candidate, Rational)] = List()
        val candidatesForBulkExclusion = selectCandidatesForBulkExclusion(totals, numRemainingVacancies, quota, bulktype, surplus)
        if (candidatesForBulkExclusion.nonEmpty) {  // DO BULK EXCLUSION  -  Section 273 (13)(b) => (13A)
         candidatesToExclude = candidatesForBulkExclusion
        }
        else {  // Exclude the least voted candidate  - Section 273 (13)(a) => (13AA)
          bulktype match {
            case ExclusionBulk => {
              val leastVotedCandidate = chooseCandidateForExclusion(totals)
              println("Candidate to be excluded: " + leastVotedCandidate )
              result.addExcludedCandidate(leastVotedCandidate._1,leastVotedCandidate._2)
              candidatesToExclude = leastVotedCandidate::candidatesToExclude
            }
            case SurplusDistributionBulk => List()
          }
        }
    candidatesToExclude
 }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 def surplusesDistribution(
   election: Election[ACTBallot], ccandidates: List[Candidate], numVacancies: Int):
    (Election[ACTBallot], List[(Candidate,Rational)]) = {
  println(" \n Distribution of surpluses. \n ")
   var newws: List[(Candidate, Rational)] = List()
   var newElection = election

   while (result.getPendingWinners.nonEmpty && newws.length != numVacancies){
    val (cand, ctotal, markings) = result.takeButRetainFirstPendingWinner  // IT IS NOT REMOVED FROM PENDING YET

    val tls = newElection.firstVotes(ccandidates)
    val candidatesToExclude = getCandidatesToExclude(tls, numVacancies, result.getQuota, SurplusDistributionBulk, Some(ctotal-result.getQuota))

    if (candidatesToExclude.nonEmpty){
      // Section 273, (13C) - HERE I DO ONLY ONE BULK EXCLUSION
      //IF IT IS POSSIBLE. CAN THERE BE ANOTHER BULK EXCLUSION AFTER THIS ONE - is unclear from (13C)

      println("\n Bulk exclusion: type 2. \n ")
      val res = exclusion(election, ccandidates, candidatesToExclude, numVacancies): (Election[ACTBallot],  List[(Candidate, Rational)] )
      newElection = res._1
      newws = newws ::: res._2
      println("Are there pending candidates? " + result.getPendingWinners.nonEmpty)
    }

    result.removePendingWinner(cand)
    val res = tryToDistributeSurplusVotes(newElection, ccandidates, cand, ctotal, markings)
      newElection = res._1
      ////printElection(newElection)
      newws = newws ::: res._2
      println("Are there pending candidates? " + result.getPendingWinners.nonEmpty)
   }
   (newElection, newws)
  }


 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// like ACT, but no fraction loss
 def tryToDistributeSurplusVotes(
   election: Election[ACTBallot], ccandidates: List[Candidate], winner: Candidate,
   ctotal:Rational, markings: Option[Set[Int]] ):
    (Election[ACTBallot], List[(Candidate,Rational)]) = {

  val pendingWinners = result.getPendingWinners.map(x => x._1)

  if (ctotal == result.getQuota) {
      val newElection = removeWinnerWithoutSurplusFromElection(election, winner)
      result.removePendingWinner(winner)
      println("Candidate with exact total is eliminated: " + winner)
      ////printElection(newElection)
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

    ////printElection(election)

    println("Distributing the surplus of " + winner)


    val surplus = ctotal - result.getQuota

    val tv = computeTransferValue(surplus, election, pendingWinners, winner, None)
    println("tv = " + tv)

    val (newElection, exhaustedBallots, ignoredBallots) = distributeSurplusVotes(election, winner, ctotal, None, pendingWinners, tv)


    val newElectionWithoutFractionInTotals = loseFraction(newElection, ccandidates)


    val newtotalsWithoutFraction = newElectionWithoutFractionInTotals.firstVotes(ccandidates)

    val newtotalsWithoutFractionWithoutpendingwinners = newtotalsWithoutFraction.filterKeys(k => !pendingWinners.contains(k))

    println("winner " + winner)

    result.removePendingWinner(winner)

    println("result.getPendingWinners " + result.getPendingWinners)

    result.addTotalsToHistory(newtotalsWithoutFractionWithoutpendingwinners)
    var ws = declareNewWinnersWhileDistributingSurpluses(newtotalsWithoutFractionWithoutpendingwinners,newElection)

        ////printElection(newElection)

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
    report.setLossByFraction(newElection.firstVotes(ccandidates), newtotalsWithoutFraction)
    ignoredBallots match { // ballots ignored because they don't belong to the last parcel of the winner
      case Some(ib) => report.setIgnoredBallots(ib)
      case None =>
    }
    //------------------------------------------------------------------

    (newElectionWithoutFractionInTotals, ws)

  }
 }

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// similar to ACT's exclusion.
// but it also implements bulk exclusion
 def exclusion(
   election: Election[ACTBallot], ccandidates: List[Candidate], candidatesForExclusion: List[(Candidate, Rational)],
   numVacancies: Int): (Election[ACTBallot],  List[(Candidate, Rational)] ) = {

     println("Vacancies left: " + numVacancies)

  var ws: List[(Candidate,Rational)] = List()
  var newws: List[(Candidate,Rational)] = List()
  var newElection = election
  var newElectionWithoutFractionInTotals = election
  var exhaustedBallots: Set[ACTBallot] = Set()

  for (candidate <- candidatesForExclusion) {

    if (candidate._2 == Rational(0,1)){
      println("Excluding candidate with zero votes: " + candidate)
      val ex = excludeZero(election, candidate._1)
      newElection = ex._1
    }
    else {
       var steps = determineStepsOfExclusion(election,candidate._1)

       while (ws.length != numVacancies && !steps.isEmpty){
         val step = steps.head
         println("Step of exclusion: " + step)
         steps = steps.tail // any better way to do this?

         val ex = exclude(newElectionWithoutFractionInTotals, step._1, Some(step._2), Some(newws.map(x => x._1)))

         newElection = ex._1
         exhaustedBallots = ex._2

         val totalsBeforeFractionLoss = newElection.firstVotes(ccandidates) // for computing LbF

         newElectionWithoutFractionInTotals = loseFraction(newElection, ccandidates) // perhaps it is better  to get rid of newws in a separate function

         val totalsAfterFractionLoss = newElectionWithoutFractionInTotals.firstVotes(ccandidates)

         val totalsWithoutNewWinners = totalsAfterFractionLoss.filterKeys(k => !ws.map(_._1).contains(k))
         // excluding winners that are already identified in the while-loop

         result.addTotalsToHistory(totalsWithoutNewWinners)

         newws =
           declareNewWinnersWhileExcluding(
             candidate._1, exhaustedBallots, totalsWithoutNewWinners,totalsWithoutNewWinners, newElectionWithoutFractionInTotals)

         ws = ws ::: newws
         report.setLossByFraction(totalsBeforeFractionLoss, totalsWithoutNewWinners)
       }
    }
  }

  var dws:  List[(Candidate, Rational)]  = List()
     if (ws.nonEmpty) {
       val res = surplusesDistribution(newElection, ccandidates.filterNot { x => candidatesForExclusion.map(_._1).contains(x) }, numVacancies - ws.length)
       newElection = res._1
       dws = res._2
     }
  (newElection, ws:::dws)
 }






}
