package countvotes.structures

import scala.language.implicitConversions

abstract class BallotBase(val id: Int, val weight: Rational)

class PreferenceBallot(val preferences: List[Candidate], override val id: Int, override val weight: Rational)
extends BallotBase(id, weight)
object PreferenceBallot {
  def apply(p: List[Candidate], id: Int, w: Rational) = new PreferenceBallot(p, id, w)
}

case class ScoreBallot(scores: List[(Candidate, Rational)], override val id: Int, w: Rational) 
extends BallotBase(id, w) 
{
  override def toString: String = "[" + id + ", " + scores + ", " + w + "]"
}

case class RankBallot(val ranks: List[(Candidate, Int)], override val id: Int, w: Rational) 
extends BallotBase(id, w) {
  override def toString: String = "[" + id + ", " + ranks + ", " + w + "]"
}


trait Value extends PreferenceBallot {
  val value: Rational
}

trait Marking extends PreferenceBallot {
  val marking: Boolean
}

class MarkedBallot(p: List[Candidate], id: Int,  m: Boolean, w: Rational)
  extends PreferenceBallot(p, id, w) with Marking {
  val marking = m
}
object MarkedBallot{
  def apply(p: List[Candidate], id: Int, m:Boolean, w: Rational): MarkedBallot = new MarkedBallot(p, id, m, w)
}
