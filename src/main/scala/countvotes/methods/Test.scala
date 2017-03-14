package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._
import collection.mutable.{HashMap => Map}
import scala.util.Random




object Test{

  def recOrderIdentical(equaltotals: List[Candidate], totalshistory: List[Map[Candidate, Rational]]): List[Candidate] = {

     println(" \n recOrderIdentical \n")
     println("equaltotals: " + equaltotals)
     println("totalshistory: " + totalshistory)
     if (totalshistory.nonEmpty) {
      var biggestcandidate: Candidate = equaltotals.head

      for (c<-equaltotals){
        if (totalshistory.head(c) > totalshistory.head(biggestcandidate)) biggestcandidate = c
      }

      println("biggest candidate: " + biggestcandidate)
      val biggestcandidates = totalshistory.head.clone() filter {p => p._2 == totalshistory.head(biggestcandidate)}
      println("biggest candidates: " + biggestcandidates)
      val lbiggestcandidates = biggestcandidates.toList.map(x => x._1)
      println("list of biggest candidates: " + lbiggestcandidates)
      val totalsofremainingcandidates = totalshistory.head.clone().retain ((k,v) => lbiggestcandidates.toSet.contains(k) == false 
        && equaltotals.toSet.contains(k) == true)
      val listoftotalsofremainingcandidates =  totalsofremainingcandidates.toList.sortBy(x => x._2).reverse
      println("listoftotalsofremainingcandidates " + listoftotalsofremainingcandidates)

      if (biggestcandidates.size > 1)
        recOrderIdentical(lbiggestcandidates, totalshistory.tail):::recOrderDifferent(
          totalsofremainingcandidates, listoftotalsofremainingcandidates, totalshistory)
      else
        lbiggestcandidates.head :: recOrderDifferent(totalsofremainingcandidates, listoftotalsofremainingcandidates, totalshistory)
     }
     else Nil
  }

  def recOrderDifferent(
    totalsOfWinners: Map[Candidate, Rational], sortedlist: List[(Candidate, Rational)], 
    totalshistory: List[Map[Candidate, Rational]]): List[Candidate] = {

    println(" \n recOrderDifferent \n")
    println("totalsOfWinners: " + totalsOfWinners)
    println("sortedlist: " + sortedlist)
    println("totalshistory: " + totalshistory)
    if (sortedlist.nonEmpty) {
     var c  = sortedlist.head
     var equaltoc = totalsOfWinners.clone() filter {_._2 == c._2}
     println("equaltoc: " + equaltoc)
     if (equaltoc.size > 1) {
       var twf = totalsOfWinners.clone() filter {_._2 != c._2}
       println("remaining candidates: " + twf)
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


  def recFindSmallest(equaltotals: Map[Candidate, Rational], totalshistory: List[Map[Candidate, Rational]]): Map[Candidate, Rational] = {

      if (equaltotals.size > 1 && totalshistory.nonEmpty) {
      val listequalcandidates = equaltotals.toList.map(x => x._1)
      var smallestcandidate: Candidate = listequalcandidates.head

      for (c<-listequalcandidates.tail){
        if (totalshistory.head(c) < totalshistory.head(smallestcandidate)) {
          smallestcandidate = c
        }
      }
      recFindSmallest(equaltotals.clone() filter { 
      p => totalshistory.head(p._1) == totalshistory.head(smallestcandidate)}, totalshistory.tail) // it may be not unique!!!
     }
     else equaltotals
   }



  def testSDResolution: Unit = {

    val totals1: Map[Candidate, Rational] =  Map(
      Candidate("A", None, None) -> 1, Candidate("B", None, None) -> 1, Candidate("C", None, None) -> 1, 
      Candidate("D", None, None) -> 1, Candidate("E", None, None) -> 2)
    val totals2: Map[Candidate, Rational] =  Map(
      Candidate("A", None, None) -> 1, Candidate("B", None, None) -> 1, Candidate("C", None, None) -> 6, 
      Candidate("D", None, None) -> 7, Candidate("E", None, None) -> 6)
    val totals3: Map[Candidate, Rational] =  Map(
      Candidate("A", None, None) -> 3, Candidate("B", None, None) -> 9, Candidate("C", None, None) -> 7, 
      Candidate("D", None, None) -> 7, Candidate("E", None, None) -> 8)
    val totals4: Map[Candidate, Rational] =  Map(
      Candidate("A", None, None) -> 19, Candidate("B", None, None) -> 10, Candidate("C", None, None) -> 10, 
      Candidate("D", None, None) -> 10, Candidate("E", None, None) -> 10)


    var totalsHistory: List[Map[Candidate, Rational]] =   totals4 :: totals3 :: totals2 :: totals1 :: List()

    println(totalsHistory)

    /*
    var sortedList = totalsHistory.head.toList.sortBy(x => x._2).reverse // >

    println(sortedList)

    val listwithtieresolved = recOrderDifferent(totalsHistory.head, sortedList, totalsHistory)
    var result = for (l <- listwithtieresolved ) yield (l, totalsHistory.head(l))

    println(result)
    */

    var min = new Rational(Int.MaxValue, 1)
    for (kv <- totalsHistory.head) if (kv._2 < min) min = kv._2
    val equaltotals = totalsHistory.head.clone() filter {_._2 == min}



    if (recFindSmallest(equaltotals, totalsHistory.tail).size > 1) {
      var  resultsmallest: (Candidate, Rational)  = Random.shuffle(equaltotals.toList).head 
      // If did not manage to resolve tie, take a random candidate (the commissioner decided according to the ACT Electorate act)

      println("The smallest candidate was not found. " +  resultsmallest + " is picked up.")

    }
    else {
     var resultsmallest: (Candidate, Rational)  = equaltotals.toList.head
      println("The smallest candidate was found: " +  resultsmallest)
    }



  }

}
