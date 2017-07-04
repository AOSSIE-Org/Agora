package countvotes.parsers

import countvotes._
import countvotes.structures._

import scala.util.parsing.combinator._

object CandidatesParser extends ElectionParser[Candidate] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
   def line : Parser[Candidate] = name ~ opt(";") ~ opt(id) ~ opt(";") ~ opt(party) ^^ {
     case  ~(~(~(~(name, _), id), _), party)  =>
       {  Candidate(name, id, party)
       }
   }

   def name : Parser[String] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { _.toString }
   def party : Parser[String] = """[0-9A-Za-z\-\,\.\ \']+""".r ^^ { _.toString }
   def id : Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

}
