package analyse

object StabilityAnalyser {

  case class AnalysisConfig(method: String = "")

  val parser = new scopt.OptionParser[AnalysisConfig]("compress") {
    head("\nCommand Line Interface for Electronic Votes preference analysis\n\n  ")

    note(
      """The arguments are as follows:""" + "\n" +
        """ -m """ + "\n \n"
    )

    opt[String]('m', "method") required() action { (v, c) =>
      c.copy(method = v)
    } text ("use stability analysis method method  <met>\n") valueName ("<met>")

    note(
      """Possible values are as follows:""" + "\n" +

        """for -m:  Borda""" + "\n")

    help("help").text("prints this usage text")
  }

  def main(args: Array[String]): Unit = {


    def callMethod(c: AnalysisConfig) = {

      c.method match {
        case "Borda" => {
          ???

        }
        case "None" => {
          println("\n\nPlease provide a method name for stability analysis.\n\n")
        }
      }
    }


    parser.parse(args, AnalysisConfig()) map { c =>
      callMethod(c)
    }

  }

}
