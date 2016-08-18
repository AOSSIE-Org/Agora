package countvotes.algorithms



import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import scala.util.Random


trait SurplusDistributionTieResolution {
  def resolveSurpluseDistributionTie(equaltotals: Map[Candidate, Rational]): List[(Candidate, Rational)]
}


trait ACTSurplusDistributionTieResolution extends STVMethod[ACTBallot] with SurplusDistributionTieResolution{
  
 def recOrderIdentical(equaltotals: List[Candidate], totalshistory: List[Map[Candidate, Rational]]): List[Candidate] = {
      
     if (totalshistory.nonEmpty) {
      var biggestcandidate: Candidate = equaltotals.head

      for (c<-equaltotals){
        if (totalshistory.head.getOrElse(c, Rational(0,1)) > totalshistory.head.getOrElse(biggestcandidate, Rational(0,1))) biggestcandidate = c
        //if (totalshistory.head(c) > totalshistory.head(biggestcandidate)) biggestcandidate = c
      }      
      val biggestcandidates = totalshistory.head.clone() filter {p => (p._2 == totalshistory.head(biggestcandidate) && equaltotals.toSet.contains(p._1) == true)}
      val lbiggestcandidates = biggestcandidates.toList.map(x => x._1)
      val totalsofremainingcandidates = totalshistory.head.clone().retain ((k,v) => lbiggestcandidates.toSet.contains(k) == false && equaltotals.toSet.contains(k) == true)
      val listoftotalsofremainingcandidates =  totalsofremainingcandidates.toList.sortBy(x => x._2).reverse 
      if (biggestcandidates.size > 1)
        recOrderIdentical(lbiggestcandidates, totalshistory.tail):::recOrderDifferent(totalsofremainingcandidates, listoftotalsofremainingcandidates, totalshistory)
      else 
        lbiggestcandidates.head :: recOrderDifferent(totalsofremainingcandidates, listoftotalsofremainingcandidates, totalshistory)  
     }
     else {
      Random.shuffle(equaltotals.toList)      // If did not manage to resolve tie, shuffle them randomly (the commissioner decided according to the ACT Electorate act) 
     }
  } 
  
  def recOrderDifferent(totalsOfWinners: Map[Candidate, Rational], sortedlist: List[(Candidate, Rational)], totalshistory: List[Map[Candidate, Rational]]): List[Candidate] = {
    if (sortedlist.nonEmpty) {
     var c  = sortedlist.head
     var equaltoc = totalsOfWinners.clone() filter {_._2 == c._2}
     if (equaltoc.size > 1) {    
       var twf = totalsOfWinners.clone() filter {_._2 != c._2} 
       if (twf.nonEmpty)
        recOrderIdentical(equaltoc.toList.map(x => x._1), totalshistory.tail):::recOrderDifferent(twf, sortedlist.filter( p => p._2 != c._2), totalshistory)
       else recOrderIdentical(equaltoc.toList.map(x => x._1), totalshistory.tail)
     }
     else    
       if (sortedlist.tail.nonEmpty)  c._1::recOrderDifferent(totalsOfWinners.clone() filter {_ != c} , sortedlist.tail, totalshistory)
       else c._1::List()
    }
    else List()
  }

  
  def resolveSurpluseDistributionTie(totalsOfWinners: Map[Candidate, Rational]): List[(Candidate, Rational)] = {
   var sortedList = totalsOfWinners.toList.sortBy(x => x._2).reverse // >
   println("sortedList: " + sortedList)
   val listwithtieresolved = recOrderDifferent(totalsOfWinners, sortedList, result.getTotalsHistory) 
   for (l <- listwithtieresolved ) yield (l, totalsOfWinners(l))
  }
  
}


trait SimpleSurplusDistributionTieResolution extends STVMethod[WeightedBallot] with SurplusDistributionTieResolution{
  def resolveSurpluseDistributionTie(equaltotals: Map[Candidate, Rational]): List[(Candidate, Rational)] = {
    equaltotals.toList.sortBy(x => x._2).reverse // >
  }
}


