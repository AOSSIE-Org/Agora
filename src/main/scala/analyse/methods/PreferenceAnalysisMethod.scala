package analyse.methods

import countvotes.structures.{PreferenceBallot => Ballot, Candidate, Election}

/**
  * Created by deepeshpandey on 18/06/17.
  */
abstract class PreferenceAnalysisMethod[B <: Ballot] {

    def analyse(e: countvotes.structures.Election[B], ccandidates: List[Candidate]): Boolean

}
