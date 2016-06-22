package countvotes.structures

import collection.mutable.{HashMap => Map}

 class Result{
   
    private var quota: Option[Rational] = None
   
    private var excludedCandidates: List[(Candidate, Rational)] = Nil
   // private var continuingCandidates: List[Candidate] = Nil
    private var pendingWinners: List[(Candidate, Rational, Set[Int])] = Nil
          
    
    def setQuota(q: Rational) = {
      quota = Some(q)
    }
    
    def getQuota: Rational = {
      quota match {
        case Some(q) => q
        case None  =>  throw new Exception("quota is not set yet.")
      }
    }
    
    def addPendingWinners(pendWinners: List[(Candidate, Rational)], markings: Set[Int]) = {
     if (pendWinners.nonEmpty)
      for (w <- pendWinners)  
       if (!pendingWinners.contains(w)) {
        //println("Adding yet undistributed winner " + w + " with markings " + markings.toList.sorted)
         addPendingWinner(w._1, w._2, markings)
       }
    }
    
    def addPendingWinner(candidate: Candidate, total: Rational, markings: Set[Int]) = {
      pendingWinners = pendingWinners :+ (candidate, total, markings) // !!! is it adding at the end of the list?
    }
    
     
    def getPendingWinners: List[(Candidate, Rational, Set[Int])] = {
     pendingWinners
    }
  
    def takeFirstPendingWinner: (Candidate, Rational, Set[Int]) = {
     val h = pendingWinners.head
     pendingWinners = pendingWinners.tail
     h
    }
    
    def removePendingWinner(c: Candidate) = { 
     pendingWinners = pendingWinners.filter(_ != c)
    }
    
   
    def addExcludedCandidate(candidate: Candidate, total: Rational) = {
     excludedCandidates = (candidate, total)::excludedCandidates
     //continuingCandidates = continuingCandidates diff List(candidate)
    }
    
    
  }