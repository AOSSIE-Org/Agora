package countvotes.parsers

import countvotes.structures.MethodParam
import play.api.libs.json.Json

import scala.io.Source

/**
  * Created by deepeshpandey on 06/08/17.
  */
object MethodParamParser {

  def parse(fileName: String): MethodParam = {

    val src = Source.fromFile(fileName).getLines.mkString
    Json.parse(src).as[MethodParam](MethodParam.methodParamReader)
  }

}
