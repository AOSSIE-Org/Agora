package countvotes.algorithms


import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}
import countvotes.methods._
import collection.mutable.{HashMap => Map}

import spire.math.Rational


trait ACTExclusion extends STV[ACTBallot] {

 def excludeZero( election: Election[ACTBallot], candidate: Candidate): (Election[ACTBallot], Set[ACTBallot] ) ={
  var list: List[ACTBallot] = Nil
  var setExhausted: Set[ACTBallot] = Set()
    for (b<-election if b.preferences.nonEmpty)
      list = ACTBallot(b.preferences.head :: b.preferences.tail filter { _ != candidate}, b.id, false, b.weight, b.value ):: list
  (Election(list), setExhausted)
 }



 def exclude( election: Election[ACTBallot],
              candidate: Candidate,
              value: Option[Rational],
              newWinners: Option[List[Candidate]]): (Election[ACTBallot], Set[ACTBallot] ) ={
  var list: List[ACTBallot] = Nil
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
             if (restOfPreferences.nonEmpty) {
               list = ACTBallot(restOfPreferences, b.id, true, b.value, b.value)::list
             }
             else {
               setExhausted += b
             }
           }
        }
        else {
          list = ACTBallot(b.preferences.head :: filterPreferences(b.preferences.tail filter {
            _ != candidate
          }, nW), b.id, false, b.weight, b.value) :: list
        }
       }
    }
  }
  (Election(list), setExhausted)
 }

}


// exactly like ACTExclusion
trait SenateExclusion extends STV[ACTBallot] {

 def excludeZero( election: Election[ACTBallot], candidate: Candidate): (Election[ACTBallot], Set[ACTBallot] ) ={
  var list: List[ACTBallot] = Nil
  var setExhausted: Set[ACTBallot] = Set()
    for (b<-election if b.preferences.nonEmpty)
      list = ACTBallot(b.preferences.head :: b.preferences.tail filter { _ != candidate}, b.id, false, b.weight, b.value ):: list
  (Election(list), setExhausted)
 }


 def exclude( election: Election[ACTBallot],
              candidate: Candidate,
              value: Option[Rational],
              newWinners: Option[List[Candidate]]): (Election[ACTBallot], Set[ACTBallot] ) ={
  var list: List[ACTBallot] = Nil
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
             else {
               setExhausted += b
             }
           }
        }
        else {
          list = ACTBallot(b.preferences.head :: filterPreferences(b.preferences.tail filter {
            _ != candidate
          }, nW), b.id, false, b.weight, b.value) :: list
        }
       }
    }
  }
  (Election(list), setExhausted)
 }






}


trait SimpleExclusion extends STV[Ballot] {

 def exclude(election: Election[Ballot],
             candidate: Candidate,
             value: Option[Rational],
             newWinners: Option[List[Candidate]]): (Election[Ballot], Set[Ballot] ) = {
   var list: List[Ballot]  = Nil
   var setExhausted: Set[Ballot] = Set()
   for (b <- election if !b.preferences.isEmpty) {
      if (b.preferences.head == candidate ) {
        if (b.preferences.tail.nonEmpty) {
          list = new Ballot(b.preferences.tail,  b.id,  b.weight)::list
        }
        else {
          setExhausted += b
        }
      }
      else {
        list = new Ballot(b.preferences.head :: b.preferences.tail filter {
          _ != candidate
        }, b.id, b.weight) :: list
      }
   }

  (Election(list), setExhausted)
 }
}

trait SimpleExclusionWithFixedElectionSize  {
  // Removes the candidate from the ballot but does not reduce the election size by removing empty ballots
  def exclude(election: Election[Ballot],
              candidate: Candidate): Election[Ballot] = {
    election map { b =>
      val newPrefs = b.preferences filter { _ != candidate }
      new Ballot(newPrefs, b.id, b.weight)
    }
  }
}