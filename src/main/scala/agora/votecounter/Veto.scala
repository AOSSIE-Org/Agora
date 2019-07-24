package agora.votecounter

import agora.model.{PreferenceBallot => Ballot, Candidate, Election}
import scala.collection.mutable.{HashMap => MMap}

object Veto extends VoteCounter[Ballot] {

  import spire.math.Rational

  // TODO: There is an implicit assumption here that all votes have weight 1.
  // Should this be checked?
  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int ):
  List[(Candidate,Rational)] = {
    val candidateScoreMap = new MMap[Candidate, Rational]
    for(c <- ccandidates) { candidateScoreMap(c) = Rational(0,1)}

    for(ballot <- election) {
      for(preference <- ballot.preferences) {
        if(!(preference == ballot.preferences.last && ballot.preferences.length > 1)) {
          candidateScoreMap(preference) = candidateScoreMap.getOrElse(preference, Rational(0, 1)) + Rational(1,1)
        }
      }
    }

    candidateScoreMap.toList.sortWith(_._2 > _._2) take (numVacancies)
  }
}
