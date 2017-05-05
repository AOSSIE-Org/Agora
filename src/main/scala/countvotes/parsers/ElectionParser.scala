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


import countvotes.structures._

import scala.io.Source
import java.io.{FileReader, FileNotFoundException, IOException}

import scala.util.parsing.combinator._


abstract class ElectionParser[T] extends RegexParsers {

  def line: Parser[T]

  def read(filename: String): List[T] = {

    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines.toList
    //val lineswithoutheading = lines.tail
    var output = for (l <- lines) yield {
      //println(l)
      //println(parse(line, l))
      parse(line, l) match {
        case Success(sucLine,_) => sucLine
        case _ => throw new Exception("Should never happen")
      }
    }
    bufferedSource.close()
    output
  }



}
