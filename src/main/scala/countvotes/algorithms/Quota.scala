package countvotes.algorithms


import countvotes.structures._


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
   BigDecimal(num.numerator / num.denominator).setScale(0, BigDecimal.RoundingMode.DOWN).toInt
  }
}
