package countvotes

import countvotes.methods._
import countvotes.parsers._
import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}

import scala.collection.mutable.{HashMap => Map}
import scala.languageFeature.implicitConversions
import scala.util.parsing.combinator._

import spire.math.Rational

abstract sealed class VoteCounterTableFormats
case object ACT extends VoteCounterTableFormats
case object Concise extends VoteCounterTableFormats


object Main extends RegexParsers {
  case class Config(directory: String = "",
                    ballotsfile: Option[String] = None,
                    method: String = "",
                    parameters: Option[Parameters] = None,
                    nvacancies: String = "",
                    nkandidates: Option[String] = None,
                    candidatesfile: String = "",
                    table: VoteCounterTableFormats = Concise)

  val parser = new scopt.OptionParser[Config]("compress") {
    head("\nCommand Line Interface for Electronic Vote Counting\n\n  ")

    note(
      """The arguments are as follows:""" + "\n" +
        """ -d [-b] -c -m [-p] -v [-k] [-t]""" + "\n \n"
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

    opt[String]('p', "parameterFile") action { (v, c) =>
      c.copy(parameters = Some(ParameterParser.parse(c.directory + v)))
    } text ("set paramfile to <pfile>\n") valueName ("<pfile>")

    opt[String]('v', "nvacancies") required() action { (v, c) =>
      c.copy(nvacancies = v)
    } text ("set number of vacancies  <numv>\n") valueName ("<numv>")

    opt[String]('k', "nkandidates") action { (v, c) =>
      c.copy(nkandidates = Some(v))
    } text ("set number of candidates  <numk>\n") valueName ("<numk>")

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

        """for -m:  EVACS, EVACSnoLP, EVACSDWD, Simple, Majority, Borda, Approval, Baldwin, Nanson, Kemeny-Young, Contingent,| Runoff2Round, Copeland, UncoveredSet, InstantExhaustiveBallot, PreferentialBlockVoting, HybridPluralityPreferentialBlockVoting, InstantExhaustiveDropOff, SAV, PAV, SPAV, Oklahoma, Meek""".stripMargin + "\n" +

        """for -t:  Concise, ACT""" + "\n \n"
    )

    help("help").text("prints this usage text")
  }


  // scalastyle:off cyclomatic.complexity
  // scalastyle:off method.length
  def main(args: Array[String]): Unit = {

    def callMethod(c: Config, electionFile: String, winnersfile: String, reportfile: String,
                   candidates_in_order: List[Candidate], parameters: Option[Parameters]) = {
      c.method match {
        case "EVACS" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new EVACS).runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          c.table match {
            case ACT => r.writeDistributionOfPreferencesACT(reportfile, Some(candidates_in_order))
            case _ => r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          }
          println("The scrutiny was recorded to " + reportfile)
          r.writeWinners(winnersfile)
          println("The winners were recorded to " + winnersfile)
        }
        case "EVACSnoLP" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new EVACSnoLP).runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          r.writeWinners(winnersfile)
        }
        case "EVACSDWD" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new EVACSDelayedWD).runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          r.writeWinners(winnersfile)
        }
        case "Senate" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          val electionwithIds = for (b <- election) yield Ballot(b.preferences, election.indexOf(b) + 1, Rational(1, 1))
          var r = (new AustralianSenate).runVoteCounter(Election.weightedElectionToACTElection(electionwithIds), candidates_in_order, c.nvacancies.toInt)
          c.table match {
            case ACT => r.writeDistributionOfPreferencesACT(reportfile, Some(candidates_in_order))
            case _ => r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          }
          println("The scrutiny was recorded to " + reportfile)
          r.writeWinners(winnersfile)
          println("The winners were recorded to " + winnersfile)
        }
        case "Simple" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new SimpleSTV).runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }
        case "Egalitarian" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = EgalitarianBrute.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }
        case "Majority" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Majority.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Approval" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = ApprovalRule.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Borda" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Borda.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Kemeny-Young" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = KemenyYoung.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Baldwin" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = BaldwinMethod.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Nanson" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Nanson.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "InstantRunoff2Round" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = InstantRunoff2Round.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }
        case "Coomb" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Coomb.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "InstantExhaustiveBallot" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = InstantExhaustiveBallot.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Contingent" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Contingent.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "RandomBallot" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = RandomBallot.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "MinimaxCondorcet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = MinimaxCondorcet.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Copeland" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Copeland.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }
        case "Dodgson" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Dodgson.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "UncoveredSet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = UncoveredSet.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "SmithSet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = SmithSet.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "InstantExhaustiveDropOff" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = InstantExhaustiveDropOffRule.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "PreferentialBlockVoting" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = PreferentialBlockVoting.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "HybridPluralityPreferentialBlockVoting" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = HybridPluralityPreferentialBlockVoting.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Oklahoma" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Oklahoma.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "SPAV" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = SequentialProportionalApprovalVoting.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "PAV" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = ProportionalApprovalVoting.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "SAV" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = SatisfactionApprovalVoting.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }
        case "SMC" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          parameters match {
            case Some(param) => {
              var r = SMC.runVoteCounter(election, candidates_in_order, param, c.nvacancies.toInt)
              r.writeWinners(winnersfile)
            }
            case None => println("\n\nPlease provide the comparison order to execute this voting method\n\n")
          }
        }

        case "RankedPairs" => {
          val election = PreferencesParserWithIndifference.read(c.directory + electionFile)
          var r = RankedPairs.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Meek" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = MeekSTV.runVoteCounter(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Schulze" => {
          val election = PreferencesParserWithRank.read(c.directory + electionFile)
          var r = Schulze.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "Maximin" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = Maximin.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          println(" VoteCounter table for method Maximin is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "RangeVoting" => {
          val election = PreferencesParserWithScore.read(c.directory + electionFile)
          var r = RangeVoting.runVoteCounter(election, candidates_in_order, c.nvacancies.toInt)
          r.writeWinners(winnersfile)
        }

        case "BipartisanSet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          parameters match {
            case Some(param) => {
              var r = BipartisanSet.runVoteCounter(election, candidates_in_order, param)
              r.writeWinners(winnersfile)
            }
            case None => println("Please provide probability distribution to compute bipartisan set")
          }
        }

        case "" => println("Please specify which algorithm should be used.")
      }
    }

    parser.parse(args, Config()) map { c =>

      c.ballotsfile match {
        case Some(filename) => { // ONLY ONE FILE IS ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          println("Candidates: " + candidates)
          val winnersfile = c.directory + "winners/" + "Winners_" + c.method + "_InputFile_" + filename
          val reportfile = c.directory + "reports/" + "Report_" + c.method + "_InputFile_" + filename
          callMethod(c, filename, winnersfile, reportfile, candidates, c.parameters)
        }
        case None => { // ALL FILES IN THE DIRECTORY ARE ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          val files = new java.io.File(c.directory).listFiles.filter(_.getName.endsWith(".kat"))
          for (file <- files) {
            val filename = file.getName
            println("------------------------------------------------")
            println("\n" + "    NEW ELECTION: " + file.getName + "\n")
            println("------------------------------------------------")
            //val election =  PreferencesParser.read(c.directory + filename)
            val winnersfile = c.directory + "winners/" + "Winners_" + c.method + "_InputFile_" + filename
            val reportfile = c.directory + "reports/" + "Report_" + c.method + "_InputFile_" + filename
            callMethod(c, filename, winnersfile, reportfile, candidates, c.parameters)
          }
        }
      }
    }
  }
}
