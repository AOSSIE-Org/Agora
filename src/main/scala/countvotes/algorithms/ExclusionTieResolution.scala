package countvotes.algorithms



import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import scala.util.Random


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait ExclusionTieResolution {
  def resolveExclusionTie(equaltotals: Map[Candidate, Rational]): (Candidate, Rational)
  
  def recFindSmallest(equaltotals: Map[Candidate, Rational], totalshistory: List[Map[Candidate, Rational]]): Map[Candidate, Rational] = {
      
     if (equaltotals.size > 1 && totalshistory.nonEmpty) {
      val listequalcandidates = equaltotals.toList.map(x => x._1)
      var smallestcandidate: Candidate = listequalcandidates.head

      for (c<-listequalcandidates){
        if (totalshistory.head(c) < totalshistory.head(smallestcandidate)) smallestcandidate
      }
      recFindSmallest(equaltotals filter {_._2 == totalshistory.head(smallestcandidate)}, totalshistory.tail)
     }
     else equaltotals
    } 
}

trait UnfairExclusionTieResolutuim {
  def resolveExclusionTie(equaltotals: Map[Candidate, Rational]): (Candidate, Rational)  = equaltotals head
}

// Todo: Count history is required here....
trait ACTExclusionTieResolution extends GenericSTVMethod[ACTBallot] with ExclusionTieResolution{
  def resolveExclusionTie(equaltotals: Map[Candidate, Rational]): (Candidate, Rational)  = {
  
    if (recFindSmallest(equaltotals, result.getTotalsHistory.tail).size > 1) {
      Random.shuffle(equaltotals.toList).head      // If did not manage to resolve tie, take a random candidate (the commissioner decided according to the ACT Electorate act) 
    }
    else 
    equaltotals.toList.head
  }
}

