package countvotes.parsers


import countvotes._
import countvotes.parsers.PreferencesParser.{candidate, id, repsep}
import countvotes.structures.Rational.realToRational
import countvotes.structures._

import scala.util.parsing.combinator._
import scala.util.matching.Regex

trait ElectionParsers extends RegexParsers {

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) } 

  def numerator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def denominator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }
  
  def weight: Parser[Rational] = numerator ~ "/" ~ denominator ^^ { case ~(~(n,_), d) => Rational(n, d) }

  def id: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def rank: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def score: Parser[Rational] = """[0-9]+""".r ^^ { case (value) => {
    val (n, d) = realToRational(value)
    Rational(n.toInt, d.toInt)}
  }
}


object PreferencesParser extends ElectionParser[WeightedBallot] with RegexParsers with ElectionParsers {
  
  def preferences: Parser[List[Candidate]] = repsep(candidate, ">")

  def line: Parser[WeightedBallot] = id ~ weight ~ preferences ^^ {
    case ~(~(i, w), prefs) => { WeightedBallot(prefs, i, w) }
  }
}

object PreferencesParserWithIndifference extends ElectionParser[RankedWeightedBallot] with RegexParsers with ElectionParsers {

  def preferences: Parser[List[(Candidate, Option[Int])]] = {

    var rank = 1
    ((candidate ^^ { case (cand) => List((cand, Some(rank)))}) ~ rep((">" ~ candidate) ^^ {
      case ~(">", cand) => {
        rank = rank + 1
        (cand, Some(rank))
      }
    } | ("=" ~ candidate) ^^ {
      case ~("=", cand) => {
        (cand, Some(rank))
      }
    })) ^^ {case ~(list1, list2) => list1 ++ list2}
  }

  def line: Parser[RankedWeightedBallot] = id ~ weight ~ preferences ^^ {
    case ~(~(i, w), prefs) => { RankedWeightedBallot(prefs, i, w) }
  }

}

object PreferencesParserWithScore extends ElectionParser[ScoredWeightedBallot] with RegexParsers with ElectionParsers {

  def candidateWithRankAndScore: Parser[(Candidate, Option[Rational])] = candidate ~ ";" ~ opt(rank) ~ ";" ~ opt(score) ^^ {
        case ~(~(~(~(candidate, ";"), rank), ";"), score) => {
          (candidate, score)
        }
      }
  
  def preferences: Parser[List[(Candidate, Option[Rational])]] = repsep(candidateWithRankAndScore, ")(")

  def line: Parser[ScoredWeightedBallot] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) => { ScoredWeightedBallot(prefs, i, w) }
  }
}

object PreferencesParserWithRank extends ElectionParser[RankedWeightedBallot] with RegexParsers with ElectionParsers {

  def candidateWithRankAndScore: Parser[(Candidate, Option[Int])] = candidate ~ ";" ~ opt(rank) ~ ";" ~ opt(score) ^^ {
    case ~(~(~(~(candidate, ";"), rank), ";"), score) => {
      (candidate, rank)
    }
  }

  def preferences: Parser[List[(Candidate, Option[Int])]] = repsep(candidateWithRankAndScore, ")(")

  def line: Parser[RankedWeightedBallot] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) => { RankedWeightedBallot(prefs, i, w) }
  }
}

