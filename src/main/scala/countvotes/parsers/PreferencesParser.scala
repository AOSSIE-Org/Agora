// Copyright (C) 2011-2012 the original author or authors.
// See the LICENCE.txt file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package countvotes.parsers


import countvotes._
import countvotes.structures._

import scala.util.parsing.combinator._


object PreferencesParser extends ElectionParser[WeightedBallot] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
   def line : Parser[WeightedBallot] = id ~ numerator ~ "/" ~ denominator ~ preferences ^^ {
     case  ~(~(~(~(i,n), "/"),d),p)  =>
       {  //println(p)
          WeightedBallot(p, i, Rational(n,d))
       }
   }

   def candidate : Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) }

   //def candidate : Parser[Candidate] = """[A-Za-z]+-? ?[A-Za-z]*\,?[A-Za-z]* ?[A-Za-z]*""".r ^^ { s => Candidate(s) }

   def preferences : Parser[List[Candidate]] = repsep(candidate, ">")

   //def weight : Parser[Double] = """[0-9]*\/?[0-9]+""".r ^^ { _.toDouble }

   def numerator : Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

   def denominator : Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

   def id : Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

}


object PreferencesWithoutIDAndWeightParser extends ElectionParser[WeightedBallot] with RegexParsers {

  // the method line returns a Parser of type ACTBallotPapersDataStructure
   def line : Parser[WeightedBallot] = preferences ^^ {
     case  p  =>
       {  println(p)
          WeightedBallot(p, 0, Rational(1,1))
       }
   }

   def candidate : Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) }


   def preferences : Parser[List[Candidate]] = repsep(candidate, ">")



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


