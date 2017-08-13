package countvotes.parsers

import countvotes.structures._

import scala.io.Source
import java.io.{FileReader, FileNotFoundException, IOException}

import scala.util.parsing.combinator._

abstract class ElectionParser[T] extends RegexParsers {

  def line: Parser[T]

  def read(filename: String): List[T] = {

    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    var output = for (l <- lines) yield {
      parse(line, l) match {
        case Success(sucLine,_) => sucLine
        case _ => throw new Exception("Parsing Error")
      }
    }
    bufferedSource.close()
    output
  }
}
