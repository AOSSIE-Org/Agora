package agora.votecounter.stv

import agora.model._
import agora.model.{PreferenceBallot => Ballot}
import agora.votecounter._
import collection.mutable.{HashMap => MMap}
import collection.Map
import scala.util.Random

import spire.math.Rational

trait SurplusDistributionTieResolution {

  def resolveSurpluseDistributionTie(
      equaltotals: Map[Candidate, Rational]
  ): List[(Candidate, Rational)]

}

// Section 273 (22)
trait SenateSurplusDistributionTieResolution
    extends STV[ACTBallot]
    with SurplusDistributionTieResolution {

  val result: Result

  def resolveSurpluseDistributionTie(
      totalsOfWinners: Map[Candidate, Rational]
  ): List[(Candidate, Rational)] = {
    val candidates          = totalsOfWinners.map(_._1).toSet
    val listwithtieresolved = recOrder(candidates, result.getTotalsHistoryClone)
    val totals              = result.getTotalsHistoryClone.head
    for (l <- listwithtieresolved) yield (l, totals(l))
  }

  def recOrder(
      candidates: Set[Candidate],
      totalshistory: List[Map[Candidate, Rational]]
  ): List[Candidate] = {
    if (candidates.nonEmpty) {
      if (totalshistory.nonEmpty) {
        val totals = totalshistory.head
        println(" totals: " + totals)
        var setOfValues: Set[Rational] = Set()
        for (candidate <- totals.filterKeys(candidates).map(_._1)) {
          if (setOfValues.contains(totals(candidate))) {
            recOrder(candidates, totalshistory.tail)
          } else {
            setOfValues = setOfValues + totals(candidate)
          }
        }
        // they are pairwise different, we just need to sort them by their totals
        totals.filter(p => candidates.contains(p._1)).toList.sortBy(x => x._2).reverse.map(_._1)
      } else { // the Australian Electoral Officer shall determine the order
        Random.shuffle(
          candidates.toList
        ) // If did not manage to resolve the tie, shuffle them randomly
      }
    } else {
      throw new Exception("Empty set of winners with equal surplus.")
    }
  }

}

trait ACTSurplusDistributionTieResolution
    extends STV[ACTBallot]
    with SurplusDistributionTieResolution {

  val result: Result

  def recOrderIdentical(
      equaltotals: List[Candidate],
      totalshistory: List[Map[Candidate, Rational]]
  ): List[Candidate] = {

    if (totalshistory.nonEmpty) {
      var biggestcandidate: Candidate = equaltotals.head
      for (c <- equaltotals) {
        if (
          totalshistory.head.getOrElse(c, Rational(0, 1)) > totalshistory.head.getOrElse(
            biggestcandidate,
            Rational(0, 1)
          )
        ) biggestcandidate = c
      }
      val biggestcandidates = totalshistory.head.filter { p =>
        p._2 == totalshistory.head(biggestcandidate) && equaltotals.toSet.contains(p._1) == true
      }
      val lbiggestcandidates = biggestcandidates.toList.map(x => x._1)
      val totalsofremainingcandidates = totalshistory.head.filterKeys(k =>
        lbiggestcandidates.toSet.contains(k) == false && equaltotals.toSet.contains(k) == true
      )
      val listoftotalsofremainingcandidates =
        totalsofremainingcandidates.toList.sortBy(x => x._2).reverse
      if (biggestcandidates.size > 1) {
        recOrderIdentical(lbiggestcandidates, totalshistory.tail) :::
          recOrderDifferent(
            totalsofremainingcandidates,
            listoftotalsofremainingcandidates,
            totalshistory
          )
      } else {
        lbiggestcandidates.head :: recOrderDifferent(
          totalsofremainingcandidates,
          listoftotalsofremainingcandidates,
          totalshistory
        )
      }
    } else {
      // If did not manage to resolve tie, shuffle them randomly (the commissioner decided according to the ACT Electorate act)
      // Random.shuffle(equaltotals.toList)
      equaltotals.sortBy(
        _.name
      ) // If did not manage to resolve tie, sort them by name (the commissioner decided according to the ACT Electorate act)
    }
  }

  def recOrderDifferent(
      totalsOfWinners: Map[Candidate, Rational],
      sortedlist: List[(Candidate, Rational)],
      totalshistory: List[Map[Candidate, Rational]]
  ): List[Candidate] = {
    if (sortedlist.nonEmpty) {
      var c        = sortedlist.head
      var equaltoc = totalsOfWinners.filter(_._2 == c._2)
      if (equaltoc.size > 1) {
        var twf = totalsOfWinners.filter(_._2 != c._2)
        if (twf.nonEmpty) {
          recOrderIdentical(
            equaltoc.toList.map(x => x._1),
            totalshistory.tail
          ) ::: recOrderDifferent(twf, sortedlist.filter(p => p._2 != c._2), totalshistory)
        } else {
          recOrderIdentical(equaltoc.toList.map(x => x._1), totalshistory.tail)
        }
      } else {
        if (sortedlist.tail.nonEmpty) {
          c._1 :: recOrderDifferent(
            totalsOfWinners.filter {
              _ != c
            },
            sortedlist.tail,
            totalshistory
          )
        } else {
          c._1 :: List()
        }
      }
    } else {
      List()
    }
  }

  def resolveSurpluseDistributionTie(
      totalsOfWinners: Map[Candidate, Rational]
  ): List[(Candidate, Rational)] = {
    val sortedList = totalsOfWinners.toList.sortBy(x => x._2).reverse // >
    // println("sortedList: " + sortedList)
    val listwithtieresolved =
      recOrderDifferent(totalsOfWinners, sortedList, result.getTotalsHistoryClone)
    for (l <- listwithtieresolved) yield (l, totalsOfWinners(l))
  }

}

trait SimpleSurplusDistributionTieResolution
    extends STV[Ballot]
    with SurplusDistributionTieResolution {

  def resolveSurpluseDistributionTie(
      equaltotals: Map[Candidate, Rational]
  ): List[(Candidate, Rational)] =
    equaltotals.toList.sortBy(x => x._2).reverse // >

}
