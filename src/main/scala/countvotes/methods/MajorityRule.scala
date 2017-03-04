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

abstract class MajorityRule[B <: WeightedBallot with Weight] extends VoteCountingMethod[B] {
  
  protected val result: Result = new Result 
  protected val report: Report[B] = new Report[B]

}

