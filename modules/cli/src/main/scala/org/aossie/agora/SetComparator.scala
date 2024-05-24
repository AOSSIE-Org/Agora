package org.aossie.agora

import org.aossie.agora.comparator.{FishburnsExtension, KellyExtension}
import org.aossie.agora.parser.{CandidatesParser, ParameterParser, PreferencesParser}
import org.aossie.agora.model.{Candidate, PreferenceBallot => Ballot, Election}

object SetComparator {

  case class CompareConfig(directory: String = "",
                              ballotsfile: Option[String] = None,
                              method: String = "",
                              parameterFile: String = "",
                              candidatesfile: String = "",
                              table: VoteCounterTableFormats = Concise)

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


    def callMethod(c: CompareConfig, election: Election[Ballot], candidates_in_order:  List[Candidate]) = {

      c.method match {
        case "Kelly" => {
          KellyExtension.compare(election, candidates_in_order, ParameterParser.parse(c.directory + c.parameterFile))

        }
        case "Fishburns" => {
          FishburnsExtension.compare(election, candidates_in_order, ParameterParser.parse(c.directory + c.parameterFile))

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
            println("\n" + "    NEW COMPARISON: " + file.getName + "\n")
            println("------------------------------------------------")
            val election = PreferencesParser.read(c.directory + filename)
            callMethod(c, election, candidates)
          }
        }
      }
    }

  }

}
