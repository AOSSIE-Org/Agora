package countvotes.structures

import collection.mutable.{HashMap => Map}

class Count[B <: Ballot with Weight]  {
    
    private var action: Option[Actions] = None  
    
    private var initiator: Option[Candidate] = None //  initiator of the action  
   
    private var election: Option[Election[B]] = None  //  election resulting from performing the action
    
    //private var modifiedelection: Option[Election[B]] = None  // outcome election resulting from performing the action and modifications, as for example lbf or random removal of ballots
   
    private var totals: Option[Map[Candidate, Rational] ] = None // outcome of the count

    private var numVotesReceived:  Option[Map[Candidate, Rational] ] = None // for each candidate, number of votes she/he received in the current count
   
    private var winners: List[(Candidate, Rational)] = Nil // outcome of the count
      
    //private var numberOfExhaustedPapers: Option[Integer] = None // outcome of the count
    
    //private var lossByFraction: Option[Rational] = None // outcome of the count
        
    //private var tv: Option[Rational] = None
    
    //private var quota: Option[Rational] = None
    
    
    /*
    def setLossByFraction(n:Rational) = {
      lossByFraction = Some(n)
    }
    
    def getLossByFraction: Rational = {
      lossByFraction match {
        case Some(n) => n
        case None => 0
      }
    }
    * */
   
    
    
    def addWinners(w: List[(Candidate, Rational)] ) = {
      winners = w.sortBy(x => x._2) ++ winners
    }
    
    def getWinners: List[(Candidate, Rational)] = {
      winners
    }
    

    def setAction(a: Actions) = {
       action = Some(a)
    }
    
    def getAction: Actions = {
      action match {
        case Some(a) => a
        case None => throw new Exception("Action is not set.")
      }
    }
   
    def setInitiator(c: Candidate) = {
      initiator = Some(c)
    }
    
    def getInitiator: Candidate = {
      initiator match {
        case Some(c) => c
        case None => new Candidate("no initiator") // TODO: fix this case
      }
      
    }
    
   def setElection(e: Election[B]) = {
      election = Some(e)
   }
   
       
    def setTotals(s: Map[Candidate, Rational]) = {
      totals = Some(s)
    }
    
    def getTotals: Map[Candidate, Rational] = {
      totals match {
        case Some(pt) => pt
        case None =>  Map() //throw new Exception("Progressive totals are not set in Report yet.")
      }
    }
    
    def setNumVotesReceived(m: Map[Candidate, Rational])  ={
      numVotesReceived = Some(m)
    }
    
    def getNumVotesReceived: Map[Candidate, Rational] = {
      numVotesReceived match {
        case Some(m) => m
        case None =>  Map() //throw new Exception("Progressive totals are not set in Report yet.")
      }
    }
    
   
}