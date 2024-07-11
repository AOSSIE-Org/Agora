package org.aossie.agora.votecounter.stv

import org.aossie.agora.model._
import org.aossie.agora.votecounter._
import spire.math.Rational

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ACT
// This function takes a lot of time. The running time is large because of it.

trait ACTFractionLoss[C <: Candidate] extends STV[C, ACTBallot] {

  def loseFraction(e: Election[C, ACTBallot], ccandidates: List[C]): Election[C, ACTBallot] = {
    val pt   = e.firstVotes(ccandidates)
    var newe = e
    for ((k, v) <- pt) {
      val n = v.toBigDecimal(0, java.math.RoundingMode.DOWN).toInt
      // println("k: " + k + "; v: " + v + "; n: " + n)
      val neweste = for (b <- newe if !b.preferences.isEmpty) yield {
        if (b.preferences.head == k) {
          if (v.numerator != 0) {
            ACTBallot(b.preferences, b.id, b.marking, b.weight * (n / v), b.value)
          } else {
            ACTBallot(b.preferences, b.id, b.marking, Rational(0, 1), Rational(0, 1))
          }
        } else {
          b
        }
      }
      newe = Election(neweste)
    }
    newe
  }

}

trait NoFractionLoss[C <: Candidate] extends STV[C, ACTBallot] {

  def loseFraction(e: Election[C, ACTBallot], ccandidates: List[C]): Election[C, ACTBallot] =
    e

}
