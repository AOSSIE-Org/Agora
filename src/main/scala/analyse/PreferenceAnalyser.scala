package analyse

import analyse.methods.{SinglePeakAnalyser, ValueRestrictedAnalyser}
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate, PreferenceBallot => Ballot, Election}
import countvotes.{Concise, VoteCounterTableFormats}


object PreferenceAnalyser {

  case class PreferenceConfig(directory: String = "",
                              ballotsfile: Option[String] = None,
                              method: String = "",
                              candidatesfile: String = "",
                              table: VoteCounterTableFormats = Concise)

  val parser = new scopt.OptionParser[PreferenceConfig]("compress") {
    head("\nCommand Line Interface for Electronic Votes preference analysis\n\n  ")

    note(
      """The arguments are as follows:""" + "\n" +
        """ -d [-b] -c -m """ + "\n \n"
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

    note(
      """Possible values are as follows:""" + "\n" +

        """for -m:  single-peak, single-caved, value-restricted""" + "\n")

    help("help").text("prints this usage text")
  }

  def main(args: Array[String]): Unit = {


    def callMethod(c: PreferenceConfig, election: Election[Ballot], candidates_in_order:  List[Candidate]) = {

      c.method match {
        case "single-peak" => {
          SinglePeakAnalyser.analyse(election, candidates_in_order)

        }
        case "single-caved" => {
          println("perform single-caved preference analysis")

        }
        case "value-restricted" => {
          ValueRestrictedAnalyser.analyse(election, candidates_in_order)
        }
      }
    }


    parser.parse(args, PreferenceConfig()) map { c =>

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
