package countvotes.parsers


import countvotes._
import countvotes.parsers.PreferencesParser.{candidate, id, repsep}
import countvotes.structures._

import scala.util.parsing.combinator._
import scala.util.matching.Regex

trait ElectionParsers extends RegexParsers {

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) } 

  def numerator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def denominator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }
  
  def weight: Parser[Rational] = numerator ~ "/" ~ denominator ^^ { case ~(~(n,_), d) => Rational(n, d) }

  def id: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }
}


object PreferencesParser extends ElectionParser[WeightedBallot] with RegexParsers with ElectionParsers {
  
  def preferences: Parser[List[Candidate]] = repsep(candidate, ">")

  def line: Parser[WeightedBallot] = id ~ weight ~ preferences ^^ {
    case ~(~(i, w), prefs) => { WeightedBallot(prefs, i, w) }
  }
}

object PreferencesParserWithIndifference extends ElectionParser[WeightedScoreRankBallot] with RegexParsers with ElectionParsers {

  def preferences: Parser[List[(Candidate, Option[Int], Option[Int])]] = {

    var rank = 1
    ((candidate ^^ { case (cand) => List((cand, Some(rank), Some(0)))}) ~ rep((">" ~ candidate) ^^ {
      case ~(">", cand) => {
        rank = rank + 1
        (cand, Some(rank), Some(0))
      }
    } | ("=" ~ candidate) ^^ {
      case ~("=", cand) => {
        (cand, Some(rank), Some(0))
      }
    })) ^^ {case ~(list1, list2) => list1 ++ list2}
  }

  def line: Parser[WeightedScoreRankBallot] = id ~ weight ~ preferences ^^ {
    case ~(~(i, w), prefs) => { WeightedScoreRankBallot(prefs, i, w) }
  }

}

object PreferencesParserWithRankAndScore extends ElectionParser[WeightedScoreRankBallot] with RegexParsers with ElectionParsers {
  
  def rank: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def score: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def candidateWithRankAndScore: Parser[(Candidate, Option[Int], Option[Int])] = candidate ~ ";" ~ opt(rank) ~ ";" ~ opt(score) ^^ {
        case ~(~(~(~(candidate, ";"), rank), ";"), score) => {
          (candidate, rank, score)
        }
      }
  
  def preferences: Parser[List[(Candidate, Option[Int], Option[Int])]] = repsep(candidateWithRankAndScore, ")(") ^^ {
    case prefs => prefs sortWith {
        case ((_, Some(r1), _), (_, Some(r2), _)) => r1 < r2        // Sorting by rank, if rank is available
        case ((_, None, Some(s1)), (_, None, Some(s2))) => s1 > s2  // Sorting by score, if rank is not available and score is
        case (_, _) => true                                         // Leaving the list unsorted, if neither rank nor score is available
    }
  }
  
  def line: Parser[WeightedScoreRankBallot] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) => { WeightedScoreRankBallot(prefs, i, w) }
  }
}

