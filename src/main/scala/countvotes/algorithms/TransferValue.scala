package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import java.io._



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 

trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallots extends GenericSTVMethod[ACTBallot]{  
  def computeTransferValue(surplus: Rational, election: Election[ACTBallot], pendingWinners:  List[Candidate], candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    var num = 0
    markings match {
     case None => throw new Exception("Last parcel is undetermined.")
     case Some(mrks) =>
      for (b <- election if !b.preferences.isEmpty) { 
        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty  && mrks.contains(b.id)) {num = num + 1}
      }
      // println("Denominator: " + num)
      surplus/num
    }
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
// TV = surplus /  total of continuing ballot papers (i.e. with further continuing preferences)
trait TransferValueWithDenominatorWithTotalOfContinuingBallots extends GenericSTVMethod[WeightedBallot]{  
  def computeTransferValue(surplus: Rational, election: Election[WeightedBallot], pendingWinners:  List[Candidate], candidate: Candidate, markings: Option[Set[Int]]): Rational = {
    var num:Rational = 0
      for (b <- election if !b.preferences.isEmpty) { 
        if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty) {num = num + b.weight}
      }
      // println("Denominator: " + num)
      surplus/num
  }
}





