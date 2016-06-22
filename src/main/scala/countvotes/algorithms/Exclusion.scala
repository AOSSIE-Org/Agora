package countvotes.algorithms


import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}

trait ACTExclusion extends GenericSTVMethod[ACTBallot] {
 
 def exclude( election: Election[ACTBallot], candidate: Candidate, value: Rational, newWinners: List[Candidate]): Election[ACTBallot] ={
  var list: Election[ACTBallot] = Nil
   for (b<-election if b.preferences.nonEmpty) {
    if (b.preferences.head == candidate && b.value == value) {
     val restOfPreferences = filterPreferences(b.preferences.tail, newWinners)
     if (restOfPreferences.nonEmpty){
       list = ACTBallot(restOfPreferences, b.id, true, b.value, b.value)::list
     }
    }
    else list = ACTBallot(b.preferences.head :: filterPreferences(b.preferences.tail filter { _ != candidate}, newWinners), b.id, false, b.weight, b.value ):: list
   }
  list
  }

}