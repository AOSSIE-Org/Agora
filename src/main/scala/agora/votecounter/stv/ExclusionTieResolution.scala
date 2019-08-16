package agora.votecounter.stv

import agora.model._
import agora.votecounter._
import collection.mutable.{HashMap => MMap}
import collection.Map
import scala.util.Random

import scala.language.postfixOps

import spire.math.Rational

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait ExclusionTieResolution {
  def chooseCandidateForExclusion(totals: Map[Candidate, Rational]): (Candidate, Rational)
}

trait UnfairExclusionTieResolution {
 def chooseCandidateForExclusion(totals: Map[Candidate, Rational]): (Candidate, Rational)   = {
   var min = Rational(Int.MaxValue, 1)
   for (kv <- totals) if (kv._2 < min) min = kv._2
   val equaltotals = totals filter {_._2 == min}
   equaltotals head
 }
}

trait PriorRoundExclusionTieResolution {
  def chooseCandidateForExclusion(equalTotals: Map[Candidate, Rational], priorRoundTotals: Map[Candidate, Rational]): (Candidate, Rational) = {
    if (equalTotals.size > 1 && priorRoundTotals.nonEmpty) {
      val equalCandidatesList = equalTotals.toList.map(x => x._1)
      var smallestCandidate: Candidate = equalCandidatesList.head
      for (c<-equalCandidatesList.tail){
        if ((priorRoundTotals.getOrElse(c, Rational(0,1))  ) < priorRoundTotals.getOrElse(smallestCandidate, Rational(0,1))) {
          smallestCandidate = c
        }
      }
      //In case there are several candidates with prior totals equal to the minimum candidate total, a random selection is made.
      Random shuffle(equalTotals filter {
        p => priorRoundTotals.getOrElse(p._1, Rational(0, 1)) == priorRoundTotals.getOrElse(smallestCandidate, Rational(0, 1))
      }) head
    }
    else {
      equalTotals head
    }
  }
}

trait ACTExclusionTieResolution extends STV[ACTBallot] with ExclusionTieResolution {
  
  val result: Result

  def recFindSmallest(equaltotals: Map[Candidate, Rational], totalshistory: List[Map[Candidate, Rational]]): Map[Candidate, Rational] = {
     if (equaltotals.size > 1 && totalshistory.nonEmpty) {
      val listequalcandidates = equaltotals.toList.map(x => x._1)
      var smallestcandidate: Candidate = listequalcandidates.head
      for (c<-listequalcandidates.tail){
        if ((totalshistory.head.getOrElse(c, Rational(0,1))  ) < totalshistory.head.getOrElse(smallestcandidate, Rational(0,1))) {
          smallestcandidate = c
        }
      }
      recFindSmallest(equaltotals filter {
        p => totalshistory.head.getOrElse(p._1, Rational(0, 1)) == totalshistory.head.getOrElse(smallestcandidate, Rational(0, 1))
      }, totalshistory.tail) // it may be not unique!!!
     }
     else {
       equaltotals
       }
   }


  def chooseCandidateForExclusion(totals: Map[Candidate, Rational]): (Candidate, Rational)  = {

    var min = Rational(Int.MaxValue, 1)
    for (kv <- totals) if (kv._2 < min) min = kv._2
    val equaltotals = totals filter {_._2 == min}
    //println("Equal smallest totals: " + equaltotals)
    val smallestCandidate = recFindSmallest(equaltotals, result.getTotalsHistoryClone.tail)
    if (smallestCandidate.size > 1) {
      // If did not manage to resolve tie, take a random candidate (the commissioner decided according to the ACT Electorate act)
      //Random.shuffle(equaltotals.toList).head
     // If did not manage to resolve tie, the candidate with the "smallest name" (the commissioner decided according to the ACT Electorate act)
     equaltotals.minBy(_._1.name)
    }
    else {
     smallestCandidate.toList.head
     //equaltotals.toList.head
    }
  }
}


