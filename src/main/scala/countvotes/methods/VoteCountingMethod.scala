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

  type MatrixD2 = Array[Array[Rational]]

  def winners(e: Election[B], ccandidates: List[Candidate], numVacancies: Int): List[(Candidate, Rational)]

  def totals(election: Election[WeightedBallot], candidates: List[Candidate]): Map[Candidate, Rational] = {
    val m = new Map[Candidate, Rational]

    for (c <- candidates) m(c) = 0

    for (b <- election if !b.preferences.isEmpty) {
      m(b.preferences.head) = b.weight + (m.getOrElse(b.preferences.head, 0))
    }
    m
  }

  def vacanciesFilled(numWinners: Int, numVacancies: Int): Boolean =
    numWinners >= numVacancies

  // When candidates' names are from 1 to N
  // Implemented to compare EVoting outputs with Jeremy's outputs
  def generateNIntCandidates(n: Integer): List[Candidate] = {
    var lcand: List[Candidate] = Nil
    for (i <- 1 to n) {
      lcand = (Candidate(i.toString(), None, None)) :: lcand
    }
    lcand
  }

  def getCandidates(election: Election[B]): List[Candidate] = {
    var set = new HashSet[Candidate]()
    for (b <- election) {
      for (c <- b.preferences)
        if (!set.exists(n => n == c)) set = set + c
    }
    set.toList
  }

  // just printing in terminal
  def printElection(election: Election[B]): Unit = {
    print("\n")
    for (e <- election.sortBy(x => x.id)) {
      var pr = ""
      for (p <- e.preferences) pr = pr + p + " > "
      println(e.id + "   " + pr.dropRight(2) + "  " + e.weight)
    }
    print("\n")
  }

  def printTotal(total: Map[Candidate, Rational]): Unit = {
    print("\n")
    for (t <- total) {
      var pr = ""
      println(t)
    }
    print("\n")
  }

  def getPairwiseComparison(election: Election[WeightedBallot], candidates: List[Candidate]): MatrixD2 = {

    val zeroRational = Rational(0, 1)
    val responseMatrix = BaseMatrix[Rational](candidates.size, candidates.size) { (i: Int, j: Int) => {
      zeroRational
    }
    }

    for (b <- election if !b.preferences.isEmpty) {
      val pref = b.preferences
      b.preferences.zipWithIndex.foreach(c1 => {
        pref.zipWithIndex.foreach(c2 => {
          if (c1._2 < c2._2) {
            responseMatrix(candidates.indexOf(c1._1))(candidates.indexOf(c2._1)) += b.weight
          }
        })
      })
    }
    responseMatrix
  }


}



