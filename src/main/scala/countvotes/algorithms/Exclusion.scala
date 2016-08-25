package countvotes.algorithms


import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}

trait ACTExclusion extends STVMethod[ACTBallot] {
 
 def excludeZero( election: Election[ACTBallot], candidate: Candidate): (Election[ACTBallot], Set[ACTBallot] ) ={
  var list: Election[ACTBallot] = Nil
  var setExhausted: Set[ACTBallot] = Set()
    for (b<-election if b.preferences.nonEmpty) 
      list = ACTBallot(b.preferences.head :: b.preferences.tail filter { _ != candidate}, b.id, false, b.weight, b.value ):: list   
  (list, setExhausted)
 }

  
  
 def exclude( election: Election[ACTBallot], candidate: Candidate, value: Option[Rational], newWinners: Option[List[Candidate]]): (Election[ACTBallot], Set[ACTBallot] ) ={
  var list: Election[ACTBallot] = Nil
  var setExhausted: Set[ACTBallot] = Set()
   value match {
    case None => throw new Exception("Argument value are missing in trait ACTExclusion")    
    case Some(v) =>  
    newWinners match { 
      case None => throw new Exception("Argument newWinners are missing in trait ACTExclusion")
      case Some(nW) =>
       for (b<-election if b.preferences.nonEmpty) {
        if (b.preferences.head == candidate && b.value == v) {
           if (b.preferences.tail.nonEmpty) {
             val restOfPreferences = filterPreferences(b.preferences.tail, candidate::nW)
             if (restOfPreferences.nonEmpty){
               list = ACTBallot(restOfPreferences, b.id, true, b.value, b.value)::list
             }
             else setExhausted += b
           }
        }
        else list = ACTBallot(b.preferences.head :: filterPreferences(b.preferences.tail filter { _ != candidate}, nW), b.id, false, b.weight, b.value ):: list
       }
    }
  }
  (list, setExhausted)
 }

}


trait SimpleExclusion extends STVMethod[WeightedBallot] {
 
 def exclude(election: Election[WeightedBallot], candidate: Candidate, value: Option[Rational], newWinners: Option[List[Candidate]]): (Election[WeightedBallot], Set[WeightedBallot] ) = {
   var list: Election[WeightedBallot]  = Nil
   var setExhausted: Set[WeightedBallot] = Set()
   for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate ) { 
        if (b.preferences.tail.nonEmpty) list = WeightedBallot(b.preferences.tail,  b.id,  b.weight)::list
        else  setExhausted += b
      }
      else list = WeightedBallot(b.preferences.head :: b.preferences.tail filter {_!= candidate}, b.id,  b.weight)::list 
   }
    
  (list, setExhausted)
 }
}
