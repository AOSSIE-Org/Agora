package countvotes.algorithms

import countvotes.structures._
import collection.mutable.{HashMap => Map}
import java.io._


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 

trait TransferValueWithDenominatorWithNumOfMarkedContinuingBallots {  
  def computeTransferValue(surplus: Rational, election: Election[Ballot], pendingWinners:  List[Candidate], candidate: Candidate, markings: Set[Int]): Rational = {
    var num = 0
    for (b <- election if !b.preferences.isEmpty) { 
      if ( b.preferences.head == candidate  && !b.preferences.tail.diff(pendingWinners).isEmpty  && markings.contains(b.id)) {num = num + 1}
    }
    // println("Denominator: " + num)
    surplus/num
  }
}

