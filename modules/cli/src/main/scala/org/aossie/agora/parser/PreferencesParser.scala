package org.aossie.agora.parser

import org.aossie.agora.model._
import org.aossie.agora.model.{PreferenceBallot => Ballot}

import spire.math.Rational

import scala.util.parsing.combinator._

trait ElectionParsers extends RegexParsers {

  def candidate: Parser[Candidate] = """[0-9A-Za-z\-\,\.\ \']*""".r ^^ { s => new Candidate(s) }

  def numerator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def denominator: Parser[BigInt] = """[0-9]*""".r ^^ { s => BigInt(s) }

  def weight: Parser[Rational] = numerator ~ "/" ~ denominator ^^ { case ~(~(n, _), d) =>
    Rational(n, d)
  }

  def id: Parser[Int] = """[0-9]+""".r ^^ { _.toInt }

  def rank: Parser[Int] =
    """[0-9]+""".r ^^ {
      _.toInt
    }

  def score: Parser[Rational] = """[0-9\.]+""".r ^^ { case value => Rational(value) }

}

object PreferencesParser
    extends ElectionParser[Candidate, Ballot]
    with RegexParsers
    with ElectionParsers {

  def preferences: Parser[List[Candidate]] = repsep(candidate, ">")

  def line: Parser[Ballot[Candidate]] = id ~ weight ~ preferences ^^ { case ~(~(i, w), prefs) =>
    new Ballot(prefs, i, w)
  }

}

object PreferencesParserWithIndifference
    extends ElectionParser[Candidate, RankBallot]
    with RegexParsers
    with ElectionParsers {

  def line: Parser[RankBallot[Candidate]] = id ~ weight ~ preferences ^^ { case ~(~(i, w), prefs) =>
    RankBallot(prefs, i, w)
  }

  def preferences: Parser[List[(Candidate, Int)]] = {

    var rank = 1
    ((candidate ^^ { cand => List((cand, rank)) }) ~ rep((">" ~ candidate) ^^ {
      case ~(">", cand) =>
        rank = rank + 1
        (cand, rank)
      case _ => throw new Exception
    } | ("=" ~ candidate) ^^ {
      case ~("=", cand) => (cand, rank)
      case _            => throw new Exception
    })) ^^ { case ~(list1, list2) => list1 ++ list2 }
  } ^^ { case prefs =>
    prefs.sortWith {
      case ((_, r1), (_, r2)) => r1 < r2
      case (_, _)             => true
    }
  }

}

object PreferencesParserWithScore
    extends ElectionParser[Candidate, ScoreBallot]
    with RegexParsers
    with ElectionParsers {

  def line: Parser[ScoreBallot[Candidate]] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) =>
      ScoreBallot(prefs, i, w)
  }

  def preferences: Parser[List[(Candidate, Rational)]] = repsep(candidateWithScore, ")(") ^^ {
    case prefs =>
      prefs.sortWith {
        case ((_, s1), (_, s2)) => s1 > s2
        case (_, _)             => true
      }
  }

  def candidateWithScore: Parser[(Candidate, Rational)] = candidate ~ ";" ~ score ^^ {
    case ~(~(candidate, ";"), score) =>
      (candidate, score)
  }

}

object PreferencesParserWithRank
    extends ElectionParser[Candidate, RankBallot]
    with RegexParsers
    with ElectionParsers {

  def line: Parser[RankBallot[Candidate]] = id ~ weight ~ opt("(") ~ preferences ~ opt(")") ^^ {
    case ~(~(~(~(i, w), _), prefs), _) =>
      RankBallot(prefs, i, w)
  }

  def preferences: Parser[List[(Candidate, Int)]] = repsep(candidateWithRank, ")(") ^^ {
    case prefs =>
      prefs.sortWith {
        case ((_, r1), (_, r2)) => r1 < r2
        case (_, _)             => true
      }
  }

  def candidateWithRank: Parser[(Candidate, Int)] = candidate ~ ";" ~ rank ^^ {
    case (~(~(candidate, ";"), rank)) =>
      (candidate, rank)
  }

}
