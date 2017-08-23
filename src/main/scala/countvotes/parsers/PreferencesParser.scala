package countvotes.parsers


import countvotes.structures.Rational.realToRational
import countvotes.structures._

import scala.util.parsing.combinator._

trait ElectionParsers extends RegexParsers {

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => Candidate(s) } 

  def numerator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def denominator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }
  
  def weight: Parser[Rational] = numerator ~ "/" ~ denominator ^^ { case ~(~(n,_), d) => Rational(n, d) }

  def id: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def rank: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def score: Parser[Rational] = """[0-9\.]+""".r ^^ { case (value) => {
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

  def preferences: Parser[List[(Candidate, Int)]] = {

    var rank = 1
    ((candidate ^^ { case (cand) => List((cand, rank))}) ~ rep((">" ~ candidate) ^^ {
      case ~(">", cand) => {
        rank = rank + 1
        (cand, rank)
      }
    } | ("=" ~ candidate) ^^ {
      case ~("=", cand) => {
        (cand, rank)
      }
    })) ^^ {case ~(list1, list2) => list1 ++ list2}
  } ^^ {
    case prefs => prefs sortWith {
      case ((_, r1), (_, r2)) => r1 < r2
      case (_,_) => true
    }
  }

  def line: Parser[RankedWeightedBallot] = id ~ weight ~ preferences ^^ {
    case ~(~(i, w), prefs) => { RankedWeightedBallot(prefs, i, w) }
  }

}

object PreferencesParserWithScore extends ElectionParser[ScoredWeightedBallot] with RegexParsers with ElectionParsers {

  def candidateWithScore: Parser[(Candidate, Rational)] = candidate ~ ";" ~ score ^^ {
        case ~(~(candidate, ";"), score) => {
          (candidate, score)
        }
      }

  def preferences: Parser[List[(Candidate, Rational)]] = repsep(candidateWithScore, ")(") ^^ {
    case prefs => prefs sortWith {
      case ((_, s1), (_, s2)) => s1 > s2
      case (_,_) => true
    }
  }

  def line: Parser[ScoredWeightedBallot] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) => { ScoredWeightedBallot(prefs, i, w) }
  }
}

object PreferencesParserWithRank extends ElectionParser[RankedWeightedBallot] with RegexParsers with ElectionParsers {

  def candidateWithRank: Parser[(Candidate, Int)] = candidate ~ ";" ~ rank ^^ {
    case (~(~(candidate, ";"), rank)) => {
      (candidate, rank)
    }
  }

  def preferences: Parser[List[(Candidate, Int)]] = repsep(candidateWithRank, ")(") ^^ {
    case prefs => prefs sortWith {
      case ((_, r1), (_, r2)) => r1 < r2
      case (_,_) => true
    }}

  def line: Parser[RankedWeightedBallot] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) => { RankedWeightedBallot(prefs, i, w) }
  }
}

