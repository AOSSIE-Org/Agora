package countvotes.structures

import scala.language.implicitConversions

// class BallotB(val id: Int, val weight: Rational)

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

class ScoreBallot(p: List[(Candidate, Rational)], id: Int, w: Rational) extends Ballot(p map {
  _._1
}, id, w) {
  val scorePreferences = p

  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}
object ScoreBallot {
  def apply(p: List[(Candidate, Rational)], id: Int, w: Rational): ScoreBallot = new ScoreBallot(p, id, w)
  implicit def toBallot(sb: ScoreBallot): Ballot = {
    new Ballot(sb.scorePreferences.map(_._1), sb.id, sb.weight)
  }
}

class RankBallot(p: List[(Candidate, Int)], id: Int, w: Rational) extends Ballot(p map {
  _._1
}, id, w) {
  val rankPreferences = p

  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}

object RankBallot {
  def apply(p: List[(Candidate, Int)], id: Int, w: Rational): RankBallot = new RankBallot(p, id, w)
  implicit def toBallot(rb: RankBallot): Ballot = {
    new Ballot(rb.rankPreferences.map(_._1), rb.id, rb.weight)
  }
}

