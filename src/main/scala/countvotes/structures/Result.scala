package countvotes.structures

import collection.mutable.{HashMap => Map}

 class Result{

    private var quota: Option[Rational] = None

    private var excludedCandidates: List[(Candidate, Rational)] = Nil
    private var pendingWinners: List[(Candidate, Rational, Option[Set[Int]])] = Nil
    private var totalsHistory: List[Map[Candidate, Rational]] = Nil  // required for ACT's ties resolutions

    private var winners: List[(Candidate, Rational)] = Nil
    private var keepFactor = new Map[Candidate, Rational]

    def clear: Unit = {
      quota = None
      excludedCandidates = Nil
      pendingWinners = Nil
      totalsHistory = Nil
      winners = Nil
    }


    def setQuota(q: Rational): Unit = {
      quota = Some(q)
    }

    def getQuota: Rational = {
      quota match {
        case Some(q) => q
        case None  =>  throw new Exception("quota is not set yet.")
      }
    }

   def setKF(kf: Map[Candidate, Rational]): Unit = {
     keepFactor = kf
   }

   def getKF: Map[Candidate, Rational] = {
     keepFactor
   }

    def addPendingWinners(pendWinners: List[(Candidate, Rational)], markings: Option[Set[Int]]): Unit = {
     if (pendWinners.nonEmpty) {
      for (w <- pendWinners)
       if (!pendingWinners.contains(w)) {
        //println("Adding yet undistributed winner " + w + " with markings " + markings.toList.sorted)
         addPendingWinner(w._1, w._2, markings)
       }
     }
    }

    def addPendingWinner(candidate: Candidate, total: Rational, markings: Option[Set[Int]]): Unit = {
      //markings match {
     //   case Some(mrks) => pendingWinners = pendingWinners :+ (candidate, total, mrks) // !!! is it adding at the end of the list?
      //  case None => pendingWinners = pendingWinners :+ (candidate, total, None) // !!! is it adding at the end of the list?
      //}
      pendingWinners = pendingWinners :+ (candidate, total, markings)
    }


    def getPendingWinners: List[(Candidate, Rational, Option[Set[Int]])] = {
     pendingWinners
    }

    def takeAndRemoveFirstPendingWinner: (Candidate, Rational, Option[Set[Int]]) = {
     val h = pendingWinners.head
     pendingWinners = pendingWinners.tail
     h
    }


    def takeButRetainFirstPendingWinner: (Candidate, Rational, Option[Set[Int]]) = {
     pendingWinners.head
    }


    def removePendingWinner(c: Candidate): Unit = {
     pendingWinners = pendingWinners.filterNot(p => p._1.name == c.name)
    }


    def addExcludedCandidate(candidate: Candidate, total: Rational): Unit = {
     excludedCandidates = (candidate, total)::excludedCandidates
     //continuingCandidates = continuingCandidates diff List(candidate)
    }

    def addTotalsToHistory(totals: Map[Candidate, Rational]): Unit = {
     totalsHistory = totals :: totalsHistory
    }

    def getTotalsHistoryClone: List[Map[Candidate, Rational]] = {
      totalsHistory map { m => m.clone() }
    }

    def setWinners(ws: List[(Candidate, Rational)]): Unit = {
      winners = ws
    }

    def getWinners: List[(Candidate, Rational)] = {
      winners
    }


  }
