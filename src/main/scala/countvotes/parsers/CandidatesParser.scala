package countvotes.parsers

import countvotes._
import countvotes.structures._

import scala.util.parsing.combinator._

object CandidatesParser extends LineParser[Candidate] {
  
  def read(filename: String) = readLines(filename)

  // the method line returns a Parser of type ACTBallotPapersDataStructure
   def line : Parser[Candidate] = name ~ opt(id) ~ opt(party) ^^ {
     case  ~(~(name, id), party)  => Candidate(name, id, party)
     case _ => throw new Exception
   }

  def string: Parser[String] = """[0-9A-Za-z\-\,\.\ \']+""".r
  def name = string ^^ { _.toString }

  //obligatory semi-colon only if party present
  def party = ";" ~ string ^^ {
    case ~(";", string) => string
    case _ => throw new Exception
  }

  //obligatory semi-colon only if id exists
  def id : Parser[Int] = ";" ~ """[0-9]+""".r ^^ {
    case ~(";", number) => number.toInt
    case _ => throw new Exception
  }

}
