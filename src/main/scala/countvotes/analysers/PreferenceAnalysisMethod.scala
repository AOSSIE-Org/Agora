package countvotes.analysers

import countvotes.structures.{Ballot, Candidate, Election, Weight}

/**
  * Created by deepeshpandey on 18/06/17.
  */
abstract class PreferenceAnalysisMethod[B <: Ballot with Weight] {

  def analyse(e: Election[B], ccandidates: List[Candidate])

}
