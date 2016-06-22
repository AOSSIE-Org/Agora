package countvotes.structures


import collection.mutable.{HashMap => Map}

  // ALWAYS MODIFYING THE LAST COUNT  (the head of list)
  class Report {
 
    
    /*
    private var countReports : List[CountReport] = Nil
    
    private var currentPTotals: Map[Candidate, Rational] = Map()
    
    private var candidates : List[Candidate] = Nil
    private var quota: Option[Rational] = None
    private var numVacancies: Option[Int] = None 
 
    
    
    def createCountReports(counts: Register, initiator: Candidate, action: Actions) = {
      if (counts.nonEmpty)
      for (c <- counts.reverse){
       val countr = new CountReport
       countr.setAction(action)  
       countr.setInitiator(initiator)  
       countr.setElection(c.election)
       countr.setProgressiveTotals(c.getProgressiveTotals)
       countr.addWinners(c.getWinners)
       countr.setValuesOfBallots(c.getValuesOfBallots)
       countReports = countr::countReports
      }
    }
    
    def setNumVacancies(n: Int) = { 
      numVacancies = Some(n)
    }
    
    def getNumVacancies = { 
      numVacancies 
    }
    
    def setCurrentPTotal(c: Candidate, v: Rational) = {
      currentPTotals(c)=v
    }
    
    def getCurrentPTotals = {
      currentPTotals
    }
        
    def setLossByFraction(n: Rational) ={
      var countr = countReports.head
      countr.setLossByFraction(n)
      countReports = countr :: countReports.tail
    }
    
    def setCandidates(cands: List[Candidate]) = {
      candidates = cands
    }
    
    
    def getCandidates: List[Candidate] = {
      candidates
    }
    
    def setQuota(q: Rational) = {
      quota = Some(q)
    }
    
    def getQuota: Rational = {
      quota match {
        case Some(q) => q
        case None  =>  throw new Exception("quota is not set yet.")
      }
    }
    
     
   def createNewCountReport(a: Actions, initiator: Option[Candidate], election: Option[Election], ptotals: Option[Map[Candidate, Rational]], bvalues: Option[Map[Int, Rational]], winners: Option[List[(Candidate, Rational)]]) = {
      
     val countr = new CountReport
     
     countr.setAction(a)
      
      initiator match {
       case Some(i) =>  countr.setInitiator(i)
       case None =>
      } 
     
      election match {
        case Some(e) => countr.setElection(e)
        case None =>
      }
      
      ptotals match {
       case Some(pt) =>  countr.setProgressiveTotals(pt)
       case None => 
      }
      
      bvalues match {
        case Some(v) => countr.setValuesOfBallots(v)
        case None =>
      }
      
      winners match {
       case Some(w) =>  countr.addWinners(w)
       case None =>
      }   

      countReports = countr::countReports
    }
    
  
     
    def getCountReports: List[CountReport] = {
      countReports
    }
   
    def setFullElection(e: Election) = {
      var countr = countReports.head
      countr.setOriginalElection(e)
      countReports = countr :: countReports.tail
    }
   
    
    def setModifiedCandidates(mc: Set[Candidate]) = {
      var countr = countReports.head
      countr.setModifiedCandidates(mc)
      countReports = countr :: countReports.tail
    }
    
    
    
    def addCountReport(cr: CountReport) = {
      countReports = cr::countReports
    }
    
    def addWinners(w: List[(Candidate, Rational)]) = {
      var count = countReports.head
      count.addWinners(w.sortBy(x => x._2))
      countReports = count :: countReports.tail
    }
    
    
    def setAction(a: Actions) = {
      var countr = countReports.head
      countr.setAction(a)
      countReports = countr :: countReports.tail
    }
    
    def setCandidateToExclude(c: (Candidate, Rational)) = {
      var count = countReports.head
      count.setCandidateToExclude(c)
      countReports = count :: countReports.tail
    }
    
    def setNumVotesReceived(m: Map[Candidate, Rational])  = {
      var count = countReports.head
      count.setNumVotesReceived(m)
      countReports = count :: countReports.tail
    }
    
    def getNumVotesReceived: Map[Candidate, Rational]  = {
      countReports.head.getNumVotesReceived
    }
    
    
    def getLossByFraction: Rational = {
       countReports.head.getLossByFraction
    }
    */
  }