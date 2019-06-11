package agora.votecounter.stv


import agora.model._

import spire.math.Rational


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait DhondtQuotientQuota {
  def computeQuota(numPartyVotes: Int, numSeatsAllocated: Int): Rational =   numPartyVotes / (numSeatsAllocated + 1)
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait DroopQuota {
  def computeQuota(numVotes: Int, numVacancies: Int): Rational =   ( numVotes / (numVacancies + 1) ) + 1
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait HagenbachBischoffQuota {
  def computeQuota(numVotes: Int, numVacancies: Int): Rational =    numVotes / (numVacancies + 1)
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait HareQuota {
  def computeQuota(numVotes: Int, numVacancies: Int): Rational =  numVotes/numVacancies
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait HuntingtonHillQuota {
  def computeQuota(numPartyVotes: Int, numSeatsAllocated: Int): Rational = {
    numPartyVotes / scala.math.sqrt(numSeatsAllocated * (numSeatsAllocated + 1))
  }
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait ImperialiQuota {
  def computeQuota(numVotes: Int, numVacancies: Int): Rational =  numVotes / (numVacancies + 2)
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


// ACT, TAS, NSW LC, NSW LG, SENATE
trait NoFractionInQuota {
  def cutQuotaFraction(num: Rational): Rational = {
   num.toBigDecimal(0, java.math.RoundingMode.DOWN).toInt
  }
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


trait WebsterSainteLagueQuota {
  def computeQuota(numPartyVotes: Int, numSeatsAllocated: Int): Rational =  numPartyVotes / (2*numSeatsAllocated + 1)
}