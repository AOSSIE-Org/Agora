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
    
    
   def newCount(action: Actions, initiator: Option[Candidate], relection: Option[Election[B]], totals: Option[Map[Candidate, Rational]], winners: Option[List[(Candidate, Rational)]]) = {
        
     val count = new Count[B]
      
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
    
    
  def writeDistributionOfPreferences(file: String) = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    
    val separator = ","
    val order = candidates
    
    // order of table headings in ACT Brundabella 2012
    /*  val order = List(new Candidate("WALL Andrew"), 
                      new Candidate("SMYTH Brendan"), 
                      new Candidate("LAWDER Nicole"), 
                      new Candidate("JEFFERY Val"), 
                      new Candidate("SESELJA Zed"), 
                      new Candidate("BRESNAN Amanda"), 
                      new Candidate("MURPHY Ben"), 
                      new Candidate("DAVIS Johnathan"), 
                      new Candidate("BURCH Joy"), 
                      new Candidate("MAFTOUM Karl"), 
                      new Candidate("GENTLEMAN Mick"), 
                      new Candidate("KINNIBURGH Mike"), 
                      new Candidate("CODY Rebecca"), 
                      new Candidate("HENSCHKE Adam"), 
                      new Candidate("ERWOOD Mark"), 
                      new Candidate("DOBLE Burl"), 
                      new Candidate("JONES-ELLIS Kieran"),
                      new Candidate("PEARCE Calvin"),
                      new Candidate("GIBBONS Mark"),
                      new Candidate("LINDFIELD Michael"))
                  
                 */
    
    writer.write( "Count" + separator) 
    var countnum = 0
    order.foreach { c => writer.write( c + separator) }
    writer.write("Loss by Fraction" + separator + "Initiator" + separator + "Action"  + separator + "Winners" +  "\n") 
    
    for (countr <- countHistory.reverse ){
          
         // println(countr.getNumVotesReceived)
      
      countnum += 1
      var line: String  = " " + separator 

      line =  countnum + separator
      
      for (c<-order){
        if ( countr.getTotals.exists(_._1 == c)) 
          line += countr.getTotals(c).numerator/countr.getTotals(c).denominator + separator
         // line += countr.getProgressiveTotals(c) + separator
        else line += separator  
      }
      
      var sw = ""
      for (w <- countr.getWinners if countr.getWinners.nonEmpty){
        sw += w._1 + " (" + w._2 + "); " 
      }
      line += "0000" + separator + separator +  separator + sw
      writer.write(line + "\n")
      
    }
    
     //rwriter.write("Candidates: " + report.getCandidates.toString()+"\n")  
     //rwriter.write("Number of seats: " + report.getNumVacancies.toString()+"\n")  
     //rwriter.write("Quota: " + report.getQuota.toString())
     writer.close()
  }
  
       
      
  }