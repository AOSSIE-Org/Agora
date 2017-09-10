package countvotes.parsers

import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}

import scala.io.Source
import java.io.{FileReader, FileNotFoundException, IOException}

import scala.util.parsing.combinator._

abstract class ElectionParser[T <: Ballot] extends LineParser[T] {

  def line: Parser[T]

  def read(filename: String): Election[T] = Election(readLines(filename))
}

abstract class LineParser[T] extends RegexParsers {

  def line: Parser[T]

  def readLines(filename: String): List[T] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    val output = for (l <- lines) yield {
      parse(line, l) match {
        case Success(sucLine,_) => sucLine
        case _ => throw new Exception("Parsing Error")
      }
    }
    bufferedSource.close()
    output
  }
}