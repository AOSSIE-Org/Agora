package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}
import java.io._


trait ACTExactWinnerRemoval extends STVMethod[ACTBallot]{
  
  
  def removeWinnerWithoutSurplusFromElection(election: Election[ACTBallot], winner: Candidate): Election[ACTBallot] = {
   var list: Election[ACTBallot] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =  ACTBallot(filterPreferences(b.preferences, winner::List()),  b.id, b.marking, b.weight, b.value)::list     
      }
   list
  }
  
}

// identical to ACTExactWinnerRemoval
trait IACTExactWinnerRemoval extends ISTVMethod[ACTBallot]{
  
  
  def removeWinnerWithoutSurplusFromElection(election: Election[ACTBallot], winner: Candidate): Election[ACTBallot] = {
   var list: Election[ACTBallot] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =  ACTBallot(filterPreferences(b.preferences, winner::List()),  b.id, b.marking, b.weight, b.value)::list     
      }
   list
  }
  
}


trait ExactWinnerRemoval extends STVMethod[WeightedBallot]{
  
  
  def removeWinnerWithoutSurplusFromElection(election: Election[WeightedBallot], winner: Candidate): Election[WeightedBallot] = {
   var list: Election[WeightedBallot] = Nil
   for (b <- election if !b.preferences.isEmpty)
      if (b.preferences.head.name != winner.name) {
        list =  WeightedBallot(filterPreferences(b.preferences, winner::List()),  b.id,  b.weight)::list     
      }
   list
  }
  
}