package countvotes.algorithms

import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}

trait NewWinnersOrderedByTotals[B <: Ballot with Weight] extends GenericSTVMethod[B]{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    val ws = totals.retain((k,v) => v >= quota)
    // val lws = ws.toSeq.sortWith(_._2 < _._2).toList
    val lws = ws.toList.sortBy(x => x._2).reverse // >
    lws
  }
}


trait NewWinnersNotOrdered[B <: Ballot with Weight] extends GenericSTVMethod[B]{
  def returnNewWinners(totals: Map[Candidate, Rational], quota: Rational): List[(Candidate,Rational)] = {
    totals.retain((k,v) => v >= quota).toList
  }
}
