package countvotes.structures

import scala.languageFeature.implicitConversions

class Ballot(val preferences: List[Candidate], val id: Int)

trait Weight extends Ballot {
  val weight: Rational
}

trait Value extends Ballot {
  val value: Rational
}

trait Marking extends Ballot {
  val marking: Boolean
}

trait Score extends Ballot {
  val scores : List[Int]
}

class WeightedBallot(p: List[Candidate], id: Int, w: Rational)
  extends Ballot(p, id) with Weight {
  val weight = w
  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}
object WeightedBallot {
  def apply(p: List[Candidate], id: Int, w: Rational): WeightedBallot  = new WeightedBallot(p, id, w)
  //def apply(p: List[Candidate], id: Int, w: Rational = new Rational(1,1)) = new SimpleBallot(p, id, w)
  //def apply(p: List[Candidate], id: Int) = new SimpleBallot(p, id, new Rational(1,1))
  // implicit def fromBallot(b: Ballot) = {
  //   new WeightedBallot(b.preferences, b.id, 1) // note that the weight is assigned 1 here
  // }
}


class MarkedWeightedBallot(p: List[Candidate], id: Int,  m: Boolean, w: Rational)
  extends WeightedBallot(p, id, w) with Marking {
  val marking = m
}
object MarkedWeightedBallot{
  def apply(p: List[Candidate], id: Int, m:Boolean, w: Rational): MarkedWeightedBallot = new MarkedWeightedBallot(p, id, m, w)
  // implicit def fromWeightedBallot(b: WeightedBallot) = {
  //   new MarkedWeightedBallot(b.preferences, b.id, true, b.weight) // note that the marking is assigned true here
  // }
}

class ScoredWeightedBallot(p: List[Candidate], id: Int, w: Rational, s: List[Int])
  extends WeightedBallot(p, id, w) with Score {
  val scores = s
  override def toString: String = "[" + id + ", " + s + ", " + p + ", " + w + "]"
}

object ScoredWeightedBallot {
  def apply(p: List[Candidate], id: Int, w: Rational, s: List[Int]): ScoredWeightedBallot  = new ScoredWeightedBallot(p, id, w, s)
}

class WeightedScoreRankBallot(p: List[(Candidate, Option[Int], Option[Int])], id: Int, w: Rational)
  extends Ballot(p map {_._1}, id) with Weight {
  val weight = w
  val scoreRankPreferences = p
  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}

object WeightedScoreRankBallot {
  def apply(p: List[(Candidate, Option[Int], Option[Int])], id: Int, w: Rational): WeightedScoreRankBallot = new WeightedScoreRankBallot(p, id, w)
}



