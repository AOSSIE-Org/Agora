package countvotes.structures

import scala.languageFeature.implicitConversions

class ACTBallot(p: List[Candidate], override val id: Int,  m:Boolean, w: Rational, v: Rational) 
extends MarkedWeightedBallot(p, id, m, w) with Value {
  val value = v
}

object ACTBallot {
  def apply(p: List[Candidate], id: Int, m:Boolean, w: Rational, v: Rational) = new ACTBallot(p,id,m,w, v)
  
  implicit def fromWeightedBallot(b: WeightedBallot) = {
    new ACTBallot(b.preferences, b.id, true, b.weight, b.weight) // note that the marking is assigned true here
  }
  
}