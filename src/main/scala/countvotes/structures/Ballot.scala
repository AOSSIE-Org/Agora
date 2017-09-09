package countvotes.structures

import scala.language.implicitConversions

class Ballot(val preferences: List[Candidate], val id: Int, val weight: Rational)
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

class ScoredBallot(p: List[(Candidate, Rational)], id: Int, w: Rational) extends Ballot(p map {
  _._1
}, id, w) {
  val scorePreferences = p

  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}

object ScoredBallot {
  def apply(p: List[(Candidate, Rational)], id: Int, w: Rational): ScoredBallot = new ScoredBallot(p, id, w)
  implicit def toBallot(sb: ScoredBallot): Ballot = {
    new Ballot(sb.scorePreferences.map(_._1), sb.id, sb.weight)
  }
}

class RankedBallot(p: List[(Candidate, Int)], id: Int, w: Rational) extends Ballot(p map {
  _._1
}, id, w) {
  val rankPreferences = p

  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}

object RankedBallot {
  def apply(p: List[(Candidate, Int)], id: Int, w: Rational): RankedBallot = new RankedBallot(p, id, w)
  implicit def toBallot(rb: RankedBallot): Ballot = {
    new Ballot(rb.rankPreferences.map(_._1), rb.id, rb.weight)
  }
}

