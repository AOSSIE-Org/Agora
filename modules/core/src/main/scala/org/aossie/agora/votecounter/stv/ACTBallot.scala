package org.aossie.agora.votecounter.stv

import scala.language.implicitConversions
import org.aossie.agora.model._
import spire.math.Rational

class ACTBallot[+C <: Candidate](
    p: List[C],
    override val id: Int,
    m: Boolean,
    w: Rational,
    v: Rational
)
//extends MarkedBallot(p, id, m, w) with Value {
    extends PreferenceBallot[C](p, id, w)
    with Value[C]
    with Marking[C] {

  val value = v

  val marking = m

}

object ACTBallot {

  def apply[C <: Candidate](
      p: List[C],
      id: Int,
      m: Boolean,
      w: Rational,
      v: Rational
  ): ACTBallot[C] =
    new ACTBallot(p, id, m, w, v)

  implicit def fromBallot[C <: Candidate](b: PreferenceBallot[C]): ACTBallot[C] = {
    new ACTBallot(
      b.preferences,
      b.id,
      true,
      b.weight,
      b.weight
    ) // note that the marking is assigned true here
  }

}

trait Value[+C <: Candidate] extends PreferenceBallot[C] {

  val value: Rational

}

trait Marking[+C <: Candidate] extends PreferenceBallot[C] {

  val marking: Boolean

}
