package countvotes.algorithms


import countvotes.structures._
import countvotes.methods._
import collection.mutable.{HashMap => Map}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ACT
// This function takes a lot of time. The running time is large because of it.

trait ACTFractionLoss extends STV[ACTBallot]{

  def loseFraction(e: Election[ACTBallot], ccandidates: List[Candidate]): Election[ACTBallot] = {
    val pt = Election.totals(e,ccandidates)
    var newe = e
    for ((k,v) <- pt) {
      val n = BigDecimal(v.numerator / v.denominator).setScale(0, BigDecimal.RoundingMode.DOWN).toInt
      //println("k: " + k + "; v: " + v + "; n: " + n)
      val neweste = for (b <- newe if !b.preferences.isEmpty) yield {
        if (b.preferences.head == k) {
          if (v.numerator != 0) {
            ACTBallot(b.preferences, b.id, b.marking, b.weight * (n / v), b.value )
          }
          else {
            ACTBallot(b.preferences, b.id, b.marking, Rational(0,1),  Rational(0,1))
          }
        }
        else {
          b
        }
      }
      newe = neweste
    }
   newe
  }

}


trait NoFractionLoss extends STV[ACTBallot]{

  def loseFraction(e: Election[ACTBallot], ccandidates: List[Candidate]): Election[ACTBallot] = {
    e
  }

}
