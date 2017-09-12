package agora.model

import java.io._
import scala.collection.mutable.{HashMap => Map}
import spire.math.Rational
import agora.votecounter.stv.Action


class Report[B <: Ballot] {

  private var countHistory : List[Count[B]] = Nil
  //private var totals: Map[Candidate, Rational] = Map()

  private var candidates : List[Candidate] = Nil
  private var quota: Option[Rational] = None
  private var numVacancies: Option[Int] = None

  private var winners: List[(Candidate,Rational)] = Nil

  private var stabilityAnalysis: List[(String, Double, Double)] = Nil

  def clear: Unit = {
    countHistory = Nil
    candidates = Nil
    quota = None
    numVacancies = None
    winners = Nil
  }

  def setLossByFractionToZero: Unit = {
    countHistory.head.setLossByFraction(Rational(0,1))
  }

  def setLossByFraction(oldtotals: Map[Candidate, Rational], newtotals:  Map[Candidate, Rational]): Unit = {

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

  def setIgnoredBallots(ignoredBallots: Election[B]): Unit = {
    countHistory.head.setIgnoredBallots(ignoredBallots)
  }

  def setNumVacancies(n: Int): Unit = {
    numVacancies = Some(n)
  }

  def getNumVacancies: Option[Int] = {
    numVacancies
  }

  def setCandidates(cands: List[Candidate]): Unit = {
    candidates = cands
  }

  def getCandidates: List[Candidate] = {
    candidates
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


 def newCount(action: Action, initiator: Option[Candidate], relection: Option[Election[B]],
   totals: Option[Map[Candidate, Rational]], winners: Option[List[(Candidate, Rational)]],
   exhaustedBallots: Option[Set[B]]): Unit = {

   val count = new Count[B]

    count.setAction(action)

    initiator match {
     case Some(i) =>  count.setInitiator(i)
     case None =>
    }

    relection match { // election resulting from the action
      case Some(e) => // count.setElection(e)
      // TODO: commented because was taking much memory. Find a better solution (make a hash table for marked ballots).
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

def setWinners(ws: List[(Candidate,Rational)] ): Unit = {
  winners = ws
}

def getWinners: List[(Candidate, Rational)]  = {
  winners
}

def writeWinners(file: String): Unit = {
 val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
   //writer.write(result.getWinners.toString())
   var sw = ""
   println("\n WINNERS \n")
   for ( w <- winners){
     println(w._1 + ": " + w._2.numerator.toFloat / w._2.denominator.toFloat + "\n")
     sw = sw + w._1 + ": " + w._2.numerator.toFloat / w._2.denominator.toFloat + "\n"
   }
   writer.write(sw)
   writer.close()
}

  def setStabilityAnalysis(sa: List[(String, Double, Double)]): Unit = {
    stabilityAnalysis = sa
  }

  def writeStabilityAnalysis(file: String): Unit = {
    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))
    //writer.write(result.getWinners.toString())
    var sw = ""
    for ( analysis <- stabilityAnalysis){
      sw = sw + analysis._1 + "\n" + "Average Ratio: " + analysis._2 + "\n" + "Variance Ratio: " + analysis._3 + "\n\n\n";
    }
    writer.write(sw)
    writer.close()
  }


 def writeDistributionOfPreferencesACT(file: String, order: Option[List[Candidate]]): Unit = {
  val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file)))

  val separator = ","

  var tableorder: List[Candidate] = Nil
  order match {
    case Some(o) =>  tableorder = o
    case None =>  tableorder = candidates
  }

  var countnum = 0

 for (count <- countHistory.reverse ){
       // println(countr.getNumVotesReceived)
   countnum += 1
   var line: String  = countnum + separator

   if (countnum == 1) {
      for (c<-tableorder){
      if (count.getTotals.exists(_._1 == c)) {
        line += count.getTotals(c).numerator/count.getTotals(c).denominator + separator
      }
      else {
        line += separator
      }
    }
    writer.write(line + "\n")
    countnum += 1
    line  = countnum + separator
   }

    for (c<-tableorder){
      if ( count.getTotals.exists(_._1 == c)) {
        line += count.getTotals(c).numerator/count.getTotals(c).denominator + separator
      }
      else {
        line += separator
      }
    }

   // val exhaustedBallots = count.getExhaustedBallots
   // val ignoredBallots = count.getIgnoredBallots

    writer.write(line + "\n")
  }
   //rwriter.write("Candidates: " + report.getCandidates.toString()+"\n")
   //rwriter.write("Number of seats: " + report.getNumVacancies.toString()+"\n")
   //rwriter.write("Quota: " + report.getQuota.toString())
   writer.close
}



  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
def writeDistributionOfPreferences(file: String, order: Option[List[Candidate]]): Unit = {
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

    countnum += 1
    var line: String  = countnum + separator

    for (c <-tableorder){
      if (count.getTotals.exists(_._1 == c)) {
        line += count.getTotals(c).numerator/count.getTotals(c).denominator + separator
      }
      else {
        line += separator
      }
    }

    var winners = ""
    for (w <- count.getWinners if count.getWinners.nonEmpty){
      winners += w._1 + " (" + w._2 + "); "
    }

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

        var s = ""

        for (b <- iB) {
          s += b.toString + "\n"
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
   writer.close
}



}

