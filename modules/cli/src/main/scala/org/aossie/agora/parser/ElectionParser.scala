package org.aossie.agora.parser

import org.aossie.agora.model._
import scala.util.parsing.combinator._

abstract class ElectionParser[C <: Candidate, T[CC >: C <: Candidate] <: Ballot[CC]]
    extends LineParser[T[C]] {

  def line: Parser[T[C]]

  def read(filename: String): Election[C, T] = Election(readLines(filename))

}

abstract class LineParser[T] extends RegexParsers {

  def line: Parser[T]

  def readLines(filename: String): List[T] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines          = bufferedSource.getLines.toList
    val output = for (l <- lines) yield {
      parse(line, l) match {
        case Success(sucLine, _) => sucLine
        case _                   => throw new Exception("Parsing Error")
      }
    }
    bufferedSource.close()
    output
  }

}
