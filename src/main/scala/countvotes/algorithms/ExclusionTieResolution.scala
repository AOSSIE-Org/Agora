package countvotes.algorithms



import countvotes.structures._
import collection.mutable.{HashMap => Map}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trait ExclusionTieResolution {
  def resolveExclusionTie(totals: Map[Candidate, Rational]): (Candidate, Rational)
}

trait UnfairExclusionTieResolutuim {
  def resolveExclusionTie(totals: Map[Candidate, Rational]): (Candidate, Rational)  = totals head
}

// Todo: Count history is required here....
trait ACTExclusionTieResolution {
  def resolveExclusionTie(totals: Map[Candidate, Rational]): (Candidate, Rational)  = {
    ???
  }
}

