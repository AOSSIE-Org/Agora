package countvotes.algorithms



import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import scala.util.Random


trait SurplusDistributionTieResolution {
  def resolveSurpluseDistributionTie(equaltotals: Map[Candidate, Rational]): List[(Candidate, Rational)]
}


trait ACTSurplusDistributionTieResolution extends GenericSTVMethod[ACTBallot] with SurplusDistributionTieResolution{
  
  def recOrder(equaltotals: List[Candidate], totalshistory: List[Map[Candidate, Rational]]): List[Candidate] = {
      
     if (totalshistory.nonEmpty) {
      //val listequalcandidates = equaltotals.toList.map(x => x._1)
      var biggestcandidate: Candidate = equaltotals.head

      for (c<-equaltotals){
        if (totalshistory.head(c) > totalshistory.head(biggestcandidate)) biggestcandidate = c
      }
      
      val biggestcandidates = totalshistory.head filter {p => p._2 == totalshistory.head(biggestcandidate)}
      val lbiggestcandidates = biggestcandidates.toList.map(x => x._1)
      
      if (biggestcandidates.size > 1)
       recOrder(lbiggestcandidates, totalshistory.tail):::recOrder(equaltotals.filterNot(lbiggestcandidates.toSet), totalshistory)
      else lbiggestcandidates.head :: recOrder(equaltotals.filterNot(lbiggestcandidates.toSet), totalshistory)
     }
     else Nil
  } 
  
  def blabla(equaltotals: Map[Candidate, Rational], sortedlist:  List[(Candidate, Rational)],  totalshistory: List[Map[Candidate, Rational]]):  List[Candidate] = {
    var c  = sortedlist.head
    var equaltoc = equaltotals filter {_._2 == c._2}
    if (equaltoc.size > 1)       
      recOrder(equaltoc.toList.map(x => x._1), totalshistory.tail):::blabla(equaltotals filter {_._2 == c._2}, sortedlist.filter( p => p._2 == c._2), totalshistory)
    else  
      c._1::blabla(equaltotals filter {_ == c} , sortedlist.tail, totalshistory)
  }
  
  
  def resolveSurpluseDistributionTie(equaltotals: Map[Candidate, Rational]): List[(Candidate, Rational)] = {
   var list = equaltotals.toList.sortBy(x => x._2).reverse // >
   val listwithtiesolved = blabla(equaltotals, list, result.getTotalsHistory) 
   for (l <- listwithtiesolved ) yield (l, equaltotals(l))
  }
}


trait SimpleSurplusDistributionTieResolution extends GenericSTVMethod[WeightedBallot] with SurplusDistributionTieResolution{
  def resolveSurpluseDistributionTie(equaltotals: Map[Candidate, Rational]): List[(Candidate, Rational)] = {
    equaltotals.toList.sortBy(x => x._2).reverse // >
  }
}


