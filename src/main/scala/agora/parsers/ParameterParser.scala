package agora.parsers

import agora.structures.Parameters
import play.api.libs.json.Json

import scala.io.Source

/**
  * Created by deepeshpandey on 06/08/17.
  */
object ParameterParser {

  def parse(fileName: String): Parameters = {

    val src = Source.fromFile(fileName).getLines.mkString
    Json.parse(src).as[Parameters](Parameters.methodParameterReader)
  }

}
