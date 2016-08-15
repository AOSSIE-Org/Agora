

package countvotes.structures

import java.io._
import collection.mutable.{HashMap => Map}



  class Report[B <: Ballot with Weight] {
 
    private var countHistory : List[Count[B]] = Nil
    //private var totals: Map[Candidate, Rational] = Map()
    
    private var candidates : List[Candidate] = Nil
    private var quota: Option[Rational] = None
    private var numVacancies: Option[Int] = None 
 
    private var winners: List[(Candidate,Rational)] = Nil
    
    
    def setLossByFractionToZero = {
      countHistory.head.setLossByFraction(0)
    }
    
    def setLossByFraction(oldtotals: Map[Candidate, Rational], newtotals:  Map[Candidate, Rational]) = {
      
        def sumTotals(totals:  Map[Candidate, Rational]): Rational = {
          var sum: Rational = 0
          for (t <- totals)  sum += t._2
          sum
        }
      
      var sumoldtotals = sumTotals(oldtotals)
      var sumnewtotals = sumTotals(newtotals)
      
      //println("sumoldtotals " + sumoldtotals)
      //println("sumnewtotals " + sumnewtotals)
      
      countHistory.head.setLossByFraction(sumoldtotals-sumnewtotals)
    }
    
    def setIgnoredBallots(ignoredBallots: Election[B]) = {
      countHistory.head.setIgnoredBallots(ignoredBallots)
    }
    
    def setNumVacancies(n: Int) = { 
      numVacancies = Some(n)
    }
    
    def getNumVacancies = { 
      numVacancies 
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
    
    
   def newCount(action: Actions, initiator: Option[Candidate], relection: Option[Election[B]], totals: Option[Map[Candidate, Rational]], winners: Option[List[(Candidate, Rational)]], exhaustedBallots: Option[Set[B]]) = {
        
     val count = new Count[B]
      
      count.setAction(action)
     
      initiator match {
       case Some(i) =>  count.setInitiator(i)
       case None =>
      } 
     
      relection match { // election resulting from the action
        case Some(e) => // count.setElection(e)  // TODO: commented because was taking much memory. Find a better solution (make a hash table for marked ballots).
        case None =>
      }
      
      totals match {
       case Some(pt) =>   count.setTotals(pt)
       case None => 
      }
      
      winners match {
       case Some(w) =>  count.addWinners(w)
       case None =>
      }   
      
      exhaustedBallots match {
        case Some(eb) => count.setExhaustedBallots(eb)
        case None => 
      }

      countHistory = count :: countHistory 
   }
    
  
     
  def getCountHistory: List[Count[B]] = {
      countHistory
  }
    
  def setWinners(ws: List[(Candidate,Rational)] ) = {
    winners = ws
  }
  
  def getWinners = {
    winners
  }
    
  def writeWinners(file: String) = {
   val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))      
     //writer.write(result.getWinners.toString())
     var sw = ""  
     for ( w <- winners){
         sw = sw + w._1 + ": " + w._2.numerator/w._2.denominator + "\n"
     }
     writer.write(sw)
     writer.close()
  }
    
    
  def writeDistributionOfPreferences(file: String, order: Option[List[Candidate]]) = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    
    val separator = ","
    
    var tableorder: List[Candidate] = Nil
    order match {
      case Some(o) =>  tableorder = o
      case None =>  tableorder = candidates
    }
                                  
    writer.write( "Count" + separator) 
    var countnum = -1
    tableorder.foreach { c => writer.write( c + separator) }
    writer.write("Initiator" + separator + 
                 "Action"  + separator + 
                 "Winners" + separator  + 
                 "~ Loss by Fraction" + separator + 
                 "N Exhausted Ballots" + separator + 
                 "~ Exhausted Votes" + separator + 
                 "N Ignored Ballots" + separator +
                 "~ Ignored Votes" 
                 + "\n") 
    
    for (count <- countHistory.reverse ){
          
         // println(countr.getNumVotesReceived)
      
      countnum += 1
      var line: String  = countnum + separator
      
      for (c<-tableorder){
        if ( count.getTotals.exists(_._1 == c)) 
          line += count.getTotals(c).numerator/count.getTotals(c).denominator + separator
         // line += count.getProgressiveTotals(c) + separator
        else line += separator  
      }
      
      var winners = ""
      for (w <- count.getWinners if count.getWinners.nonEmpty){
        winners += w._1 + " (" + w._2 + "); " 
      }
      
      //println("Exhausted ballots: " + count.getExhaustedBallots)
      line +=   count.getInitiator + separator + count.getAction + separator + winners + separator + count.getLossByFraction.toInt   + separator 
     
      
      val exhaustedBallots = count.getExhaustedBallots
      val ignoredBallots = count.getIgnoredBallots
      
      
     
      var exhhaustedandignored: String = ""
      exhaustedBallots match {
        case Some (eB) => 
          var totalweighteB: Rational = 0
          for (b <- eB) {
           totalweighteB += b.weight
          }
         exhhaustedandignored +=   eB.size + separator + totalweighteB.toInt + separator
        case None =>
          exhhaustedandignored +=   " " + separator 
      }
     ignoredBallots match {
        case Some (iB) => 
          
          val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file + "IgnoredBallots_Count" + countnum + ".txt")))      
           //writer.write(result.getWinners.toString())
          
          var s = ""
          
          for (b <- iB) {
            s += b.id + " " + b.preferences + " " + b.weight + "\n" 
          }
          
           writer.write(s)
           writer.close()
          
          
          var totalweightiB: Rational = 0
          for (b <- iB) {
           totalweightiB += b.weight
          }
        exhhaustedandignored += iB.size + separator + totalweightiB.toInt
        case None => 
     }
          
     line += exhhaustedandignored
      writer.write(line + "\n")
      
      
    }
    
     //rwriter.write("Candidates: " + report.getCandidates.toString()+"\n")  
     //rwriter.write("Number of seats: " + report.getNumVacancies.toString()+"\n")  
     //rwriter.write("Quota: " + report.getQuota.toString())
     writer.close
  }
  
       
      
  }

