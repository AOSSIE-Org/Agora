package analyse.methods

import countvotes.structures.{Ballot, Candidate, Election}

/**
  * Created by deepeshpandey on 18/06/17.
  */
abstract class PreferenceAnalysisMethod[B <: Ballot] {

    def analyse(e: Election[B], ccandidates: List[Candidate]): Boolean

}
