package countvotes.structures

import scala.language.implicitConversions

abstract class BallotBase(val id: Int, val weight: Rational)

class Ballot(val preferences: List[Candidate], override val id: Int, override val weight: Rational)
extends BallotBase(id, weight)
object Ballot {
  def apply(p: List[Candidate], id: Int, w: Rational): Ballot  = new Ballot(p, id, w)
}

trait Value extends Ballot {
  val value: Rational
}

trait Marking extends Ballot {
  val marking: Boolean
}

class MarkedBallot(p: List[Candidate], id: Int,  m: Boolean, w: Rational)
  extends Ballot(p, id, w) with Marking {
  val marking = m
}
object MarkedBallot{
  def apply(p: List[Candidate], id: Int, m:Boolean, w: Rational): MarkedBallot = new MarkedBallot(p, id, m, w)
}

class ScoreBallot(val scores: List[(Candidate, Rational)], id: Int, w: Rational) 
extends Ballot(
  scores map { _._1}, 
  id, 
  w
) 
{
  override def toString: String = "[" + id + ", " + scores + ", " + w + "]"
}
object ScoreBallot {
  def apply(p: List[(Candidate, Rational)], id: Int, w: Rational): ScoreBallot = new ScoreBallot(p, id, w)
}

class RankBallot(val ranks: List[(Candidate, Int)], id: Int, w: Rational) extends Ballot(ranks map {
  _._1
}, id, w) {

  override def toString: String = "[" + id + ", " + ranks + ", " + w + "]"
}

object RankBallot {
  def apply(p: List[(Candidate, Int)], id: Int, w: Rational): RankBallot = new RankBallot(p, id, w)
}

