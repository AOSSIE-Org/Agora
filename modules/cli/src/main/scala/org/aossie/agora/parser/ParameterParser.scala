package org.aossie.agora.parser

import org.aossie.agora.model.Parameters
import play.api.libs.json.Json

import scala.io.Source

/** Created by deepeshpandey on 06/08/17. */
object ParameterParser {

  def parse(fileName: String): Parameters = {

    val src = Source.fromFile(fileName)
    val parameters =
      Json.parse(src.getLines.mkString).as[Parameters](Parameters.methodParameterReader)
    src.close()
    parameters
  }

}
