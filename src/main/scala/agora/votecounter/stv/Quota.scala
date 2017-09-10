package agora.votecounter.stv


import agora.model._

import spire.math.Rational


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait DroopQuota {
  def computeQuota(numVotes: Int, numVacancies: Int): Rational =   ( numVotes / (numVacancies + 1) ) + 1
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait HareQuota {
  def computeQuota(numVotes: Int, numVacancies: Int): Rational =  numVotes/numVacancies
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


// ACT, TAS, NSW LC, NSW LG, SENATE
trait NoFractionInQuota {
  def cutQuotaFraction(num: Rational): Rational = {
   num.toBigDecimal(0, java.math.RoundingMode.DOWN).toInt
  }
}
