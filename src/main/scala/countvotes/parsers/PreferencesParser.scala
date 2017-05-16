package countvotes.parsers


import countvotes._
import countvotes.structures._

import scala.util.parsing.combinator._
import scala.util.matching.Regex

object PreferencesParser extends ElectionParser[WeightedBallot] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
  def line: Parser[WeightedBallot] = id ~ numerator ~ "/" ~ denominator ~ preferences ^^ {
    case ~(~(~(~(i, n), "/"), d), p) => {
      //println(p)
      WeightedBallot(p, i, Rational(n, d))
    }
  }

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) }

  //def candidate : Parser[Candidate] = """[A-Za-z]+-? ?[A-Za-z]*\,?[A-Za-z]* ?[A-Za-z]*""".r ^^ { s => Candidate(s) }

  def preferences: Parser[List[Candidate]] = repsep(candidate, ")(")

  //def weight : Parser[Double] = """[0-9]*\/?[0-9]+""".r ^^ { _.toDouble }

  def numerator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def denominator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def id: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }


}

object PreferencesParserWithRankAndScore extends ElectionParser[WeightedBallot] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
  // the method line returns a Parser of type ACTBallotPapersDataStructure


  def line: Parser[WeightedBallot] = id ~ numerator ~ "/" ~ denominator ~ "(" ~ preferences ~ ")" ^^ {
    case ~(~(~(~(~(~(i, n), "/"), d), "("), p), ")") => {
      //println(p)
      def rank: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

      def score: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

      def line2: Parser[List[(Candidate, Int, Int)]] = candidate ~ ";" ~ rank ~ ";" ~ score ^^ {
        case ~(~(~(~(candidate, ";"), rank), ";"), score) => {
          List((candidate, rank, score))
        }
      }

      var temp: List[(Candidate, Int, Int)] = Nil

      for (i <- p if !p.isEmpty) {
        parse(line2, i) match {
          case Success(sucLine, _) => temp = sucLine ::: temp
          case _ => throw new Exception("Should never happen")
        }
      }

      temp = temp.sortWith(_._2 < _._2)

      var ccand: List[Candidate] = Nil

      for (f <- temp if !temp.isEmpty) {
        ccand = f._1 :: ccand
      }

      WeightedBallot(ccand.reverse, i, Rational(n, d))
    }
  }

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) }

  //def candidate : Parser[Candidate] = """[A-Za-z]+-? ?[A-Za-z]*\,?[A-Za-z]* ?[A-Za-z]*""".r ^^ { s => Candidate(s) }

  def choice: Parser[String] = """[0-9A-Za-z\-\,\;\.\ \']*""".r ^^ { _.toString }

  def preferences: Parser[List[String]] = repsep(choice, ")(")

  //def weight : Parser[Double] = """[0-9]*\/?[0-9]+""".r ^^ { _.toDouble }

  def numerator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def denominator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def id: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }


}


object PreferencesWithoutIDAndWeightParser extends ElectionParser[WeightedBallot] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
  def line: Parser[WeightedBallot] = preferences ^^ {
    case p => {
      println(p)
      WeightedBallot(p, 0, Rational(1, 1))
    }
  }

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) }


  def preferences: Parser[List[Candidate]] = repsep(candidate, ">")


}


/*

object PreferencesParser extends ElectionParser[Ballot] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
   def line : Parser[Ballot] = id ~ weight ~ preferences ^^ {
     case  ~(~(i,w),p)  =>
       { //println(p)
         new Ballot(p, w, i)
       }
   }

   def candidate : Parser[Candidate] = """[A-Za-z\-\,\ ]*""".r ^^ { s => Candidate(s) }

   //def candidate : Parser[Candidate] = """[A-Za-z]+-? ?[A-Za-z]*\,?[A-Za-z]* ?[A-Za-z]*""".r ^^ { s => Candidate(s) }

   def preferences : Parser[List[Candidate]] = repsep(candidate, ">")

   def weight : Parser[Double] = """[0|1][.][0-9]+""".r ^^ { _.toDouble }

   def id : Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

}
*/
