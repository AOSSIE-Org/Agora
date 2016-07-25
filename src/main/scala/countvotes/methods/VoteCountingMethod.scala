package countvotes.methods


import countvotes.structures._
import countvotes.algorithms._


import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import scala.collection.SortedMap
import collection.mutable.HashSet
import collection.breakOut
import scala.util.Random
import scala.util.Sorting
import java.io._


abstract class VoteCountingMethod[B <: Ballot with Weight] {

  
  def vacanciesFilled(numWinners:Int, numVacancies:Int): Boolean = 
    numWinners >= numVacancies
  
 
 
  
 def getCandidates(election: Election[B]): List[Candidate] = {
   var set = new HashSet[Candidate]()
   for (b <- election) {
     for (c <- b.preferences)
       if (!set.exists(n => n == c) ) set = set + c 
    }
   set.toList
  }
    
       
 // just printing in terminal
 def printElection(election: Election[B]) = {
    print("\n")
    for (e <- election.sortBy(x => x.id)) {
      var pr = ""
      for (p <- e.preferences) pr = pr + p + " > "
      println(e.id + "   " + pr.dropRight(2) + "  " + e.weight)
    }
    print("\n")
 }
  
    
}