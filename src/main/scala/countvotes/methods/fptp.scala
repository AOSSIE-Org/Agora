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

abstract class fptp[B <: WeightedBallot with Weight] extends VoteCountingMethod[B] {

  protected val result: Result = new Result
  protected val report: Report[B] = new Report[B]


   def computeTotals(election: Election[B], candidates: List[Candidate]): Map[Candidate, Rational] = {

     val m = new Map[Candidate, Rational]

      for (c<-candidates) m(c) = 0

      for (b <- election if !b.preferences.isEmpty) {
        m(b.preferences.head) = b.weight + (m.getOrElse(b.preferences.head, 0))
      }
     m
  }

}
