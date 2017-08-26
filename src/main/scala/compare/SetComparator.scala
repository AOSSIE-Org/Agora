package compare

import compare.extensions.KellyExtension
import countvotes.analysers.{SinglePeakAnalyser, ValueRestrictedAnalyser}
import countvotes.parsers.{CandidatesParser, ParameterParser, PreferencesParser}
import countvotes.structures.{Candidate, WeightedBallot}
import countvotes.{Concise, ScrutinyTableFormats}

object SetComparator {

  case class CompareConfig(directory: String = "",
                              ballotsfile: Option[String] = None,
                              method: String = "",
                              parameterFile: String = "",
                              candidatesfile: String = "",
                              table: ScrutinyTableFormats = Concise)

  val parser = new scopt.OptionParser[CompareConfig]("compress") {
    head("\nCommand Line Interface for candidates set comparisons\n\n  ")

    note(
      """The arguments are as follows:""" + "\n" +
        """ -d [-b] -c -m -p """ + "\n \n"
    )

    opt[String]('d', "directory") required() unbounded() action { (v, c) =>
      c.copy(directory = v)
    } text ("set working directory to <dir>\n") valueName ("<dir>")

    opt[String]('b', "ballotsfile") action { (v, c) =>
      c.copy(ballotsfile = Some(v))
    } text ("use preferences listed in <bfile>\n") valueName ("<bfile>")

    opt[String]('c', "candidatesfile") required() action { (v, c) =>
      c.copy(candidatesfile = v)
    } text ("use preferences listed in <cfile>\n") valueName ("<cfile>")

    opt[String]('m', "method") required() action { (v, c) =>
      c.copy(method = v)
    } text ("use preference analysis method method  <met>\n") valueName ("<met>")

    opt[String]('p', "parameterFile") required() action { (v, c) =>
      c.copy(parameterFile = v)
    } text ("set paramfile to <pfile>\n") valueName ("<pfile>")

    note(
      """Possible values are as follows:""" + "\n" +

        """for -m:  Kelly, Fishburns""" + "\n")

    help("help").text("prints this usage text")
  }

  def main(args: Array[String]): Unit = {


    def callMethod(c: CompareConfig, election: List[WeightedBallot], candidates_in_order:  List[Candidate]) = {

      c.method match {
        case "Kelly" => {
          KellyExtension.compare(election, candidates_in_order, ParameterParser.parse(c.directory + c.parameterFile))

        }
        case "Fishburns" => {
          println("perform single-caved preference analysis")

        }
      }
    }


    parser.parse(args, CompareConfig()) map { c =>

      c.ballotsfile match {
        case Some(filename) => { // ONLY ONE FILE IS ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          val election = PreferencesParser.read(c.directory + filename)
          callMethod(c, election, candidates)
        }
        case None => { // ALL FILES IN THE DIRECTORY ARE ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          val files = new java.io.File(c.directory).listFiles.filter(_.getName.endsWith(".kat"))
          for (file <- files) {
            val filename = file.getName
            println("------------------------------------------------")
            println("\n" + "    NEW ELECTION: " + file.getName + "\n")
            println("------------------------------------------------")
            val election = PreferencesParser.read(c.directory + filename)
            callMethod(c, election, candidates)
          }
        }
      }
    }

  }

}
