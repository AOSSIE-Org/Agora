package countvotes

import countvotes.parsers._
import countvotes.structures._
import countvotes.algorithms._
import countvotes.methods._

import scala.collection.immutable.ListMap
import collection.mutable.{HashMap => Map}
import collection.mutable.HashSet
import java.io._

import scala.languageFeature.implicitConversions

import countvotes.structures.ACTBallot
import countvotes.structures.Election

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.io.Source

abstract sealed class ScrutinyTableFormats
  case object ACT extends ScrutinyTableFormats
  case object Concise extends ScrutinyTableFormats


object Main extends RegexParsers {

  case class Config(directory: String = "",
                    ballotsfile: Option[String] = None,
                    method: String = "",
                    nvacancies: String = "",
                    //order: String = "",
                    nkandidates: Option[String] = None,
                    // for numerical ordering and for having candidates not occuring in the elections
                    // sufficient when candidates' names are integers from 1 to ncandidates
                    // TODO: for a general case, a list of all candidates has to be provided
                    candidatesfile: String = "",
                    table: ScrutinyTableFormats = Concise)

  val parser = new scopt.OptionParser[Config]("compress") {
    head("\nCommand Line Interface for Electronic Vote Counting\n\n  ")

    note(
      """The arguments are as follows:""" + "\n" +
        """ -d [-b] -c -m -v [-k] [-t]""" + "\n \n"
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
    } text ("use vote counting method  <met>\n") valueName ("<met>")

    opt[String]('v', "nvacancies") required() action { (v, c) =>
      c.copy(nvacancies = v)
    } text ("set number of vacancies  <numv>\n") valueName ("<numv>")

    opt[String]('k', "nkandidates") action { (v, c) =>
      c.copy(nkandidates = Some(v))
    } text ("set number of candidates  <numk>\n") valueName ("<numk>")

    //opt[String]('o', "order") action { (v, c) =>
    //  c.copy(order = v)
    //} text("set order in which the candidates appear in output tables <ord>\n") valueName("<ord>")

    opt[String]('t', "table") action { (v, c) => {

      val tableFormat = v match {
        case "ACT" => ACT
        case _ => Concise
      }
      c.copy(table = tableFormat)
    }

    } text ("set format of the output table <tbl>\n") valueName ("<tbl>")

    note(
      """Possible values are as follows:""" + "\n" +

        """for -m:  EVACS, EVACSnoLP, EVACSDWD, Simple, Majority, Borda, Approval, Baldwin, Nanson, Kemeny-Young, Contingent, Runoff2Round, Copeland""" + "\n" +

        """for -t:  Concise, ACT""" + "\n \n"
    )

    help("help").text("prints this usage text")
  }


  def main(args: Array[String]): Unit = {

    def fileFormatDenoter(c: Config, filename: String): List[WeightedBallot] = {
      def name: Parser[String] =
        """[0-9A-Za-z\-\_]*""".r ^^ {
          _.toString
        }

      def extension: Parser[String] =
        """[a-z]*""".r ^^ {
          _.toString
        }

      def filenameParser: Parser[List[WeightedBallot]] = name ~ "." ~ extension ^^ {
        case ~(~(name, "."), extension) => {
          extension match {
            case "e" => {
              PreferencesParser.read(c.directory + filename)
            }
            case "es" => {
              PreferencesParserWithRankAndScore.read(c.directory + filename)
            }
            case "er" => {
              PreferencesParserWithRankAndScore.read(c.directory + filename)
            }
            case "esr" => {
              PreferencesParserWithRankAndScore.read(c.directory + filename)
            }
          }
        }
      }

      parse(filenameParser, filename) match {
        case Success(sucLine, _) => sucLine
        case _ => throw new Exception("Should never happen")
      }
    }

    def callMethod(c: Config, election: List[WeightedBallot], winnersfile: String, reportfile: String, candidates_in_order: List[Candidate]) = {
      c.method match {
        case "EVACS" => {
          var r = (new EVACSMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          c.table match {
            case ACT => r.writeDistributionOfPreferencesACT(reportfile, Some(candidates_in_order))
            case _ => r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          }
          println("The scrutiny was recorded to " + reportfile)
          r.writeWinners(winnersfile)
          println("The winners were recorded to " + winnersfile)
        }
        case "EVACSnoLP" => {
          var r = (new EVACSnoLPMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          r.writeWinners(winnersfile)
        }
        case "EVACSDWD" => {
          var r = (new EVACSDelayedWDMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          r.writeWinners(winnersfile)
        }
        case "Senate" => {
          val electionwithIds = for (b <- election) yield WeightedBallot(b.preferences, election.indexOf(b) + 1, Rational(1, 1))
          var r = (new SenateMethod).runScrutiny(Election.weightedElectionToACTElection(electionwithIds), candidates_in_order, c.nvacancies.toInt)
          c.table match {
            case ACT => r.writeDistributionOfPreferencesACT(reportfile, Some(candidates_in_order))
            case _ => r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          }
          println("The scrutiny was recorded to " + reportfile)
          r.writeWinners(winnersfile)
          println("The winners were recorded to " + winnersfile)
        }
        case "Simple" => {
          var r = (new SimpleSTVMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Simple is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "Egalitarian" => {
          var r = EgalitarianMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Egalitarian is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "Majority" => {
          var r = MajorityRuleMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Majority is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Approval" => {
          var r = ApprovalRule.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Approval is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Borda" => {
          var r = BordaRuleMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Borda is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Kemeny-Young" => {
          var r = KemenyYoungMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Kemeny-Young is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Baldwin" => {
          var r = BaldwinMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Baldwin is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Nanson" => {
          var r = NansonMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Nanson is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "InstantRunoff2Round" => {
          var r = InstantRunoff2Round.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Runoff2Round is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "Coomb" => {
          var r = CoombRuleMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println("Scrutinity table for method Coomb is not implemented yet")
          r.writeWinners(winnersfile)
        }

        case "Contingent" => {
          var r = ContingentMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Contingent is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "RandomBallot" => {
          var r = RandomBallotMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Random Ballot is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "MinimaxCondorcet" => {
          var r = MinimaxCondorcetMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Random Ballot is not implemented yet.")
          r.writeWinners(winnersfile)

        }
        case "Copeland" => {
          var r = CopelandMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Copeland is not implemented yet. ")
          r.writeWinners(winnersfile)

        }
        case "Dodgson" => {
          var r = DodgsonMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Random Ballot is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Test" => {
          Test.testSDResolution
        }
        case "" => println("Please, specify which algorithm should be used. Only option -m EVACS is currently stable.")
      }
    }

    parser.parse(args, Config()) map { c =>

      c.ballotsfile match {
        case Some(filename) => { // ONLY ONE FILE IS ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          println("Candidates: " + candidates)
          val election = fileFormatDenoter(c, filename)
          //val election =  PreferencesParser.read(c.directory + filename)
          //val election = PreferencesParserWithoutRankWithScore.read(c.directory + filename)
          //val election =  PreferencesParserWithRankWithoutScore.read(c.directory + filename)
          //val election =  PreferencesParserWithRankAndScore.read(c.directory + filename)
          //println("Election: " + election)
          val winnersfile = c.directory + "winners/" + "Winners_" + c.method + "_InputFile_" + filename
          val reportfile = c.directory + "reports/" + "Report_" + c.method + "_InputFile_" + filename
          callMethod(c, election, winnersfile, reportfile, candidates)
        }
        case None => { // ALL FILES IN THE DIRECTORY ARE ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          val files = new java.io.File(c.directory).listFiles.filter(_.getName.endsWith(".kat"))
          for (file <- files) {
            val filename = file.getName
            println("------------------------------------------------")
            println("\n" + "    NEW ELECTION: " + file.getName + "\n")
            println("------------------------------------------------")
            val election = fileFormatDenoter(c, filename)
            //val election =  PreferencesParser.read(c.directory + filename)
            val winnersfile = c.directory + "winners/" + "Winners_" + c.method + "_InputFile_" + filename
            val reportfile = c.directory + "reports/" + "Report_" + c.method + "_InputFile_" + filename
            callMethod(c, election, winnersfile, reportfile, candidates)
          }
        }
      }
    }
  }
}

