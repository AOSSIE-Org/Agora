package analyse.methods

import countvotes.structures.{Candidate, Election, PreferenceBallot => Ballot}

import scala.collection.mutable.ListBuffer

/**
  * Please check the last page of the pdf https://drive.google.com/file/d/0B4uPp6wWiMpSZ2FaTFFneGtJSDg/view
  * when preferences are single peaked there is always a unique condorcet winner
  */
object SinglePeakAnalyser extends PreferenceAnalysisMethod[Ballot] {

  def analyse(election: countvotes.structures.Election[Ballot], candidates: List[Candidate]): Boolean = {

    require(election.forall(b => b.preferences.size == candidates.size))

    getSinglePeakAxis(election, candidates) match {
      case Some(axis) => {
        if (isCompatibleAxis(axis, election)) {
          println("Single Peaked with respect to ", axis.mkString(" > "))
          true
        } else {
          println("\n\nNot Single Peaked!\n\n")
          false
        }
      }
      case None => {
        println("\n\nNot Single Peaked!\n\n")
        false
      }
    }
  }

  /**
    * returns a single peak linear ordering if it exists otherwise None
    * @param election
    * @param candidates
    * @return
    */
  def getSinglePeakAxis(election: Election[Ballot], candidates: List[Candidate]): Option[List[Candidate]] = {

    val B = election.map(_.preferences.last).distinct

    // size of B cannot be zero as ensured by the required condition
    if (B.size > 2) {
      None
    } else {
      val left = new ListBuffer[Candidate]
      val right = new ListBuffer[Candidate]

      if (B.size == 2) {
        left.insert(0, B.head)
        right.insert(0, B.last)
      } else {
        left.insert(0, B.head)
      }
      singlePeakAxisAux(left, right, election, candidates.filter(!B.contains(_)))
    }
  }


  /**
    * recursive utility which finds the last ranked candidates and set L and set R and places those candidates in left/right lists
    * and then recurse
    * @param left
    * @param right
    * @param election
    * @param candidates
    * @return
    */
  def singlePeakAxisAux(left: ListBuffer[Candidate], right: ListBuffer[Candidate], election: Election[Ballot],
                        candidates: List[Candidate]): Option[List[Candidate]] = {

    if (candidates.isEmpty) {

      Option((left ++ right).toList)

    } else if (candidates.size == 1) {

      left.insert(left.size, candidates.head)
      Option((left ++ right).toList)

    } else {
      // process only when there are atleast 2 candidates that needs to be placed
      val B = election.filter(b => b.preferences.nonEmpty && b.preferences.exists(c => !(left ++ right).contains(c)))
        .flatMap(_.preferences.reverseIterator.filter(c => !(left ++ right).contains(c)).take(1)).toSet

      val L = B.filter(x => existNrxl(election, x, right.headOption, left.lastOption, candidates))
      val R = B.filter(x => existNrxl(election, x, left.lastOption, right.headOption, candidates))

      // the algorithm condition
      if (B.size <= 2 && L.size <= 1 && R.size <= 1 && L.intersect(R).isEmpty) {

        val leftPlaceCandidate = (B -- R).union(L).toList.headOption
        val rightPlaceCandidate = (B -- Set(leftPlaceCandidate.get)).union(R).toList.headOption

        // left place candidate will always exist after the above expression
        left.insert(left.size, leftPlaceCandidate.get)

        if (rightPlaceCandidate.nonEmpty) {
          right.insert(0, rightPlaceCandidate.get)
        }
        singlePeakAxisAux(left, right, election, candidates.filter(!B.contains(_)))

      } else {
        None
      }
    }

  }

  /**
    * returns true if there exists a voter who has x as last ranked candidate and also prefer candidate l > x > r (algorithm reference : Set Nlxr)
    * @param election
    * @param x
    * @param l
    * @param r
    * @param candidates
    * @return
    */
  def existNrxl(election: Election[Ballot], x: Candidate, l: Option[Candidate], r: Option[Candidate],
                candidates: List[Candidate]): Boolean = {

    // get all the preferences where candidate x is last ranked
    val xLastRankedList = election.map(_.preferences)
      .filter(_.reverseIterator.filter(c => candidates.contains(c)).toList.headOption == Option(x))

    // only 3 possible conditions
    if (l.nonEmpty && r.nonEmpty) {
      xLastRankedList.exists(prefs => prefs.indexOf(l.get) < prefs.indexOf(x) && prefs.indexOf(x) < prefs.indexOf(l.get))
    } else if (l.isEmpty && r.nonEmpty) {
      xLastRankedList.exists(prefs => prefs.indexOf(x) < prefs.indexOf(r.get))
    } else {
      xLastRankedList.exists(prefs => prefs.indexOf(l.get) < prefs.indexOf(x))
    }
  }


  /**
    * this checks if calculated axis is compatible with all the ballots
    * as per definition 2 of http://www.lamsade.dauphine.fr/~lang/papers/elo-ecai08.pdf
    * @param axis - single peak ordering
    * @param election - election
    * @return
    */
  def isCompatibleAxis(axis: List[Candidate], election : Election[Ballot]): Boolean = {

    election.forall(b => {

      val prefs = b.preferences
      val peak = b.preferences.head

      prefs.tail.zipWithIndex.forall(c1 => {
        prefs.tail.zipWithIndex.filter(c => c._2 > c1._2 && {
          // check if c1 and c2 are on the same side of the axis
          if (axis.indexOf(prefs.head) < axis.indexOf(c1._1) && axis.indexOf(prefs.head) < axis.indexOf(c._1)
            || axis.indexOf(prefs.head) > axis.indexOf(c1._1) && axis.indexOf(prefs.head) > axis.indexOf(c._1)) {
            true
          } else{
            false
          }
        }).forall(c2 => {
            if ((axis.indexOf(peak) > axis.indexOf(c1._1) &&  axis.indexOf(c1._1) > axis.indexOf(c2._1)) ||
              (axis.indexOf(c2._1) > axis.indexOf(c1._1) &&  axis.indexOf(c1._1) > axis.indexOf(peak))) {
              true
            } else {
              false
            }
        })
      })
    })
  }

}
