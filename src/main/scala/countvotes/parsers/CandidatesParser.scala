package countvotes.parsers

import countvotes._
import countvotes.structures._

import scala.util.parsing.combinator._


object CandidatesParser extends ElectionParser[Candidate] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
   def line : Parser[Candidate] = name ^^ {
     case  n  =>
       {  n
       }
   }

   def name : Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) }

}
