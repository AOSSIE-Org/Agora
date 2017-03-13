

package countvotes.structures

import collection.mutable.{HashMap => Map}

class Count[B <: Ballot with Weight]  {

    private var action: Option[Actions] = None

    private var initiator: Option[Candidate] = None //  initiator of the action

    private var election: Option[Election[B]] = None  //  election resulting from performing the action

    //private var modifiedelection: Option[Election[B]] = None  
    // outcome election resulting from performing the action and modifications, as for example lbf or random removal of ballots

    private var totals: Option[Map[Candidate, Rational] ] = None // outcome of the count

    //private var numVotesReceived:  Option[Map[Candidate, Rational] ] = None // for each candidate, number of votes she/he received in the current count

    private var winners: List[(Candidate, Rational)] = Nil // outcome of the count

    private var exhaustedBallots: Option[Set[B]] = None // outcome of the count

    private var lossByFraction: Option[Rational] = None   // outcome of the count

    private var ignoredBallots: Option[Election[B]] = None // outcome of  the count

    //private var tv: Option[Rational] = None

    //private var quota: Option[Rational] = None


    def setExhaustedBallots(exhballots: Set[B]): Unit = {
      exhaustedBallots = Some(exhballots)
    }

    def getExhaustedBallots: Option[Set[B]] = {
      exhaustedBallots
    }

    def setIgnoredBallots(iballots: Election[B]): Unit = {
      ignoredBallots = Some(iballots)
    }

    def getIgnoredBallots: Option[Election[B]] = {
      ignoredBallots
    }

    def setLossByFraction(lbf: Rational): Unit = {
      lossByFraction = Some(lbf)
    }

    def getLossByFraction: Rational = {
      lossByFraction match {
        case Some(lbf) => lbf
        case None => throw new Exception("Loss by Fraction is not set.")
      }
    }

    def addWinners(w: List[(Candidate, Rational)] ): Unit = {
      winners = w.sortBy(x => x._2) ++ winners
    }

    def getWinners: List[(Candidate, Rational)] = {
      winners
    }


    def setAction(a: Actions): Unit = {
       action = Some(a)
    }

    def getAction: Actions = {
      action match {
        case Some(a) => a
        case None => throw new Exception("Action is not set.")
      }
    }

    def setInitiator(c: Candidate): Unit = {
      initiator = Some(c)
    }

    def getInitiator: Candidate = {
      initiator match {
        case Some(c) => c
        case None => new Candidate("no initiator")
      }

    }

   def setElection(e: Election[B]): Unit = {
      election = Some(e)
   }


    def setTotals(s: Map[Candidate, Rational]): Unit = {
      totals = Some(s)
    }

    def getTotals: Map[Candidate, Rational] = {
      totals match {
        case Some(pt) => pt
        case None =>  Map() //throw new Exception("Progressive totals are not set in Report yet.")
      }
    }

    /*
    def setNumVotesReceived(m: Map[Candidate, Rational])  ={
      numVotesReceived = Some(m)
    }

    def getNumVotesReceived: Map[Candidate, Rational] = {
      numVotesReceived match {
        case Some(m) => m
        case None =>  Map() //throw new Exception("Progressive totals are not set in Report yet.")
      }
    }
    *
    */


}

