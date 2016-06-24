package countvotes.algorithms



import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import scala.util.Random


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait ExclusionTieResolution {
  def resolveExclusionTie(equaltotals: Map[Candidate, Rational]): (Candidate, Rational)
}

trait UnfairExclusionTieResolutuim {
  def resolveExclusionTie(equaltotals: Map[Candidate, Rational]): (Candidate, Rational)  = equaltotals head
}

// Todo: Count history is required here....
trait ACTExclusionTieResolution extends GenericSTVMethod[ACTBallot] with ExclusionTieResolution{
  
  def recFindSmallest(equaltotals: Map[Candidate, Rational], totalshistory: List[Map[Candidate, Rational]]): Map[Candidate, Rational] = {
      
     if (equaltotals.size > 1 && totalshistory.nonEmpty) {
      val listequalcandidates = equaltotals.toList.map(x => x._1)
      var smallestcandidate: Candidate = listequalcandidates.head

      for (c<-listequalcandidates.tail){
        if (totalshistory.head(c) < totalshistory.head(smallestcandidate)) {
          smallestcandidate = c
        }
      }
      recFindSmallest(equaltotals filter { p => totalshistory.head(p._1) == totalshistory.head(smallestcandidate)}, totalshistory.tail) // it may be not unique!!!
     }
     else equaltotals
   } 
  
  
  def resolveExclusionTie(equaltotals: Map[Candidate, Rational]): (Candidate, Rational)  = {
  
    if (recFindSmallest(equaltotals, result.getTotalsHistory.tail).size > 1) {
      Random.shuffle(equaltotals.toList).head      // If did not manage to resolve tie, take a random candidate (the commissioner decided according to the ACT Electorate act) 
    }
    else 
    equaltotals.toList.head
  }
}

