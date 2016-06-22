package countvotes.algorithms

import countvotes.structures._
import collection.mutable.{HashMap => Map}
import java.io._


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 

trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallots {  
  def computeTransferValue(surplus: Rational, election: Election[Ballot], pendingWinners:  List[Candidate], candidate: Candidate, markings: Option[Set[Int]]): Rational = {
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

