package countvotes.structures

import scala.languageFeature.implicitConversions

class Ballot(val preferences: List[Candidate], val id: Int)
class ScoredBallot(val preferences: List[(Candidate,Int)], val id: Int)

trait Weight extends Ballot {
  val weight: Rational
}

trait Value extends Ballot {
  val value: Rational
}

trait Marking extends Ballot {
  val marking: Boolean
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

class WeightedScoredBallot(p: List[(Candidate,Int)], id: Int, w: Rational)
  extends ScoredBallot(p, id) with Weight {
  val weight = w
  override def toString: String = "[" + id + ", " + p + ", " + w + "]"
}

object WeightedScoredBallot {
  def apply(p: List[(Candidate,Int)], id: Int, w: Rational): WeightedScoredBallot  = new WeightedScoredBallot(p, id, w)
}


class MarkedWeightedScoredBallot(p: List[(Candidate,Int)], id: Int,  m: Boolean, w: Rational)
  extends WeightedScoredBallot(p, id, w) with Marking {
  val marking = m
}
object MarkedWeightedScoredBallot{
  def apply(p: List[(Candidate,Int)], id: Int, m:Boolean, w: Rational): MarkedWeightedScoredBallot = new MarkedWeightedScoredBallot(p, id, m, w)
}

