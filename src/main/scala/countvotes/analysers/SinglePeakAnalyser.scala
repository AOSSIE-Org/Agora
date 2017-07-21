package countvotes.analysers

import countvotes.structures.{Candidate, Election, WeightedBallot}
import scala.collection.mutable.ListBuffer

/**
  * Please check the last page of the pdf https://drive.google.com/file/d/0B4uPp6wWiMpSZ2FaTFFneGtJSDg/view
  * when preferences are single peaked there is always a unique condorcet winner
  */
object SinglePeakAnalyser extends PreferenceAnalysisMethod[WeightedBallot] {

  def analyse(election: Election[WeightedBallot], candidates: List[Candidate]): Boolean = {

    require(election.forall(b => b.preferences.size == candidates.size))

    val singlePeakOrdering = getSinglePeakAxis(election, candidates)
     singlePeakOrdering match {
      case Some(axis) => {
        if (checkSinglePeakAxis(axis, election)) {
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
    * using the recursive util - singlePeakAxisUtil it returns the linear ordering if found or None if not
    * @param election
    * @param candidates
    * @return
    */
  def getSinglePeakAxis(election: Election[WeightedBallot], candidates: List[Candidate]): Option[List[Candidate]] = {

    val B = election.map(_.preferences.reverseIterator.toList.head).toSet

    if (B.size > 2) {
      None
    } else {
      val left = new ListBuffer[Candidate]
      val right = new ListBuffer[Candidate]
      B.size match {
        case 2 => {
          left.insert(0, B.toList.head)
          right.insert(0, B.toList.last)
        }
        case 1 =>
          left.insert(0, B.toList.head)
      }
      singlePeakAxisUtil(left, right, election, candidates.filter(!B.contains(_)))
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
  def singlePeakAxisUtil(left: ListBuffer[Candidate], right: ListBuffer[Candidate], election: Election[WeightedBallot],
                         candidates: List[Candidate]): Option[List[Candidate]] = {

    candidates.size match {
      case 0 => {Option((left ++ right).toList)}
      case 1 => {left.insert(left.size, candidates.head); Option((left ++ right).toList)}
      case _ => {
        val B = election.filter(b => b.preferences.nonEmpty && b.preferences.exists(c => !(left ++ right).contains(c)))
          .flatMap(_.preferences.reverseIterator.filter(c => !(left ++ right).contains(c)).take(1)).toSet

        val L = B.filter(x => existNrxl(election, x, right.headOption, left.lastOption, candidates))
        val R = B.filter(x => existNrxl(election, x, left.lastOption, right.headOption, candidates))

        if (B.size <= 2 && L.size <= 1 && R.size <= 1 && L.intersect(R).isEmpty) {

          val leftPlaceCandidate = (B -- R).union(L).toList.headOption
          val rightPlaceCandidate = (B -- Set(leftPlaceCandidate.get)).union(R).toList.headOption

          // left place candidate will always exist after the above expression
          left.insert(left.size, leftPlaceCandidate.get)

          if (rightPlaceCandidate.nonEmpty) {
            right.insert(0, rightPlaceCandidate.get)
          }
          singlePeakAxisUtil(left, right, election, candidates.filter(!B.contains(_)))

        } else {
          None
        }
      }
    }
  }

  /**
    * return true if there are voters who have x as last ranked candidate also have a preference l > x > r
    * @param election
    * @param x
    * @param l
    * @param r
    * @param candidates
    * @return
    */
  def existNrxl(election: Election[WeightedBallot], x: Candidate, l: Option[Candidate], r: Option[Candidate],
                candidates: List[Candidate]): Boolean = {

    val xLastRankedList = election.map(_.preferences)
      .filter(_.reverseIterator.filter(c => candidates.contains(c)).toList.headOption == Option(x))

    // only 3 possible conditions
    if (l.nonEmpty && r.nonEmpty) {
      xLastRankedList.exists(prefs => prefs.indexOf(l.get) < prefs.indexOf(x) && prefs.indexOf(x) < prefs.indexOf(l.get))
    } else {
      if(l.isEmpty && r.nonEmpty) {
        xLastRankedList.exists(prefs => prefs.indexOf(x) < prefs.indexOf(r.get))
      } else {
        xLastRankedList.exists(prefs => prefs.indexOf(l.get) < prefs.indexOf(x))
      }
    }
  }


  /**
    * this checks if calculated axis is compatible with all the ballots
    * as per definition 2 of http://www.lamsade.dauphine.fr/~lang/papers/elo-ecai08.pdf
    * @param axis - single peak ordering
    * @param election - election
    * @return
    */
  def checkSinglePeakAxis(axis: List[Candidate], election : Election[WeightedBallot]): Boolean = {

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
