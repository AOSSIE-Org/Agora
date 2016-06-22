package countvotes.algorithms


import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}

trait ACTExclusion extends GenericSTVMethod[ACTBallot] {
 
 def exclude( election: Election[ACTBallot], candidate: Candidate, value: Option[Rational], newWinners: Option[List[Candidate]]): Election[ACTBallot] ={
  var list: Election[ACTBallot] = Nil
   value match {
    case None => throw new Exception("Argument value are missing in trait ACTExclusion")    
    case Some(v) =>  
    newWinners match { 
      case None => throw new Exception("Argument newWinners are missing in trait ACTExclusion")
      case Some(nW) =>
       for (b<-election if b.preferences.nonEmpty) {
        if (b.preferences.head == candidate && b.value == v) {
         val restOfPreferences = filterPreferences(b.preferences.tail, nW)
         if (restOfPreferences.nonEmpty){
           list = ACTBallot(restOfPreferences, b.id, true, b.value, b.value)::list
         }
        }
        else list = ACTBallot(b.preferences.head :: filterPreferences(b.preferences.tail filter { _ != candidate}, nW), b.id, false, b.weight, b.value ):: list
       }
    }
  }
  list
 }

}


trait SimpleExclusion extends GenericSTVMethod[WeightedBallot] {
 
 def exclude(election: Election[WeightedBallot], candidate: Candidate, value: Option[Rational], newWinners: Option[List[Candidate]]): Election[WeightedBallot] = {
   var list: Election[WeightedBallot]  = Nil 
   for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate ) { 
        list = WeightedBallot(b.preferences.tail,  b.id,  b.weight)::list
      }
      //else Ballot(b.preferences filter {_ != ctv._1}, b.weight, b.id)
      else list = WeightedBallot(b.preferences.head :: b.preferences.tail filter {_!= candidate}, b.id,  b.weight)::list 
   }
    
  list
 }
}
