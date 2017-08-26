package countvotes

import countvotes.methods._
import countvotes.parsers._
import countvotes.structures.{Election, _}

import scala.collection.mutable.{HashMap => Map}
import scala.languageFeature.implicitConversions
import scala.util.parsing.combinator._

abstract sealed class ScrutinyTableFormats
  case object ACT extends ScrutinyTableFormats
  case object Concise extends ScrutinyTableFormats


object Main extends RegexParsers {

  case class Config(directory: String = "",
                    ballotsfile: Option[String] = None,
                    method: String = "",
                    parameterFile: Option[String] = None,
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
      c.copy(parameterFile = Some(v))
    } text ("set paramfile to <pfile>\n") valueName ("<pfile>")

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

        """for -m:  EVACS, EVACSnoLP, EVACSDWD, Simple, Majority, Borda, Approval, Baldwin, Nanson, Kemeny-Young, Contingent,| Runoff2Round, Copeland, UncoveredSet, InstantExhaustiveBallot, PreferentialBlockVoting, HybridPluralityPreferentialBlockVoting, InstantExhaustiveDropOff, SAV, PAV, SPAV, Oklahoma""".stripMargin + "\n" +

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
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new EVACSnoLPMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          r.writeWinners(winnersfile)
        }
        case "EVACSDWD" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new EVACSDelayedWDMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          r.writeDistributionOfPreferences(reportfile, Some(candidates_in_order))
          r.writeWinners(winnersfile)
        }
        case "Senate" => {
          val election = PreferencesParser.read(c.directory + electionFile)
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
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = (new SimpleSTVMethod).runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Simple is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "Egalitarian" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = EgalitarianMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Egalitarian is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "Majority" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = MajorityRuleMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Majority is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Approval" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = ApprovalRule.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Approval is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Borda" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = BordaRuleMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Borda is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Kemeny-Young" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = KemenyYoungMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Kemeny-Young is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Baldwin" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = BaldwinMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Baldwin is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Nanson" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = NansonMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Nanson is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "InstantRunoff2Round" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = InstantRunoff2Round.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Runoff2Round is not implemented yet.")
          r.writeWinners(winnersfile)
        }
        case "Coomb" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = CoombRuleMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println("Scrutinity table for method Coomb is not implemented yet")
          r.writeWinners(winnersfile)
        }

        case "InstantExhaustiveBallot" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = InstantExhaustiveBallot.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method ExhaustiveBallot is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Contingent" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = ContingentMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Contingent is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "RandomBallot" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = RandomBallotMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Random Ballot is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "MinimaxCondorcet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = MinimaxCondorcetMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Random Ballot is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Copeland" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = CopelandMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Copeland is not implemented yet. ")
          r.writeWinners(winnersfile)
        }

        case "UncoveredSet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = UncoveredSetMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println("Scrutinity table for method Uncovered set is not implemented yet")
          r.writeWinners(winnersfile)
        }

        case "SmithSet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = SmithSetMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Runoff2Round is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "InstantExhaustiveDropOff" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = InstantExhaustiveDropOffRule.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Exhaustive Ballot with Drop off is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "PreferentialBlockVoting" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = PreferentialBlockVoting.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Preferential block voting is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "HybridPluralityPreferentialBlockVoting" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = HybridPluralityPreferentialBlockVoting.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Preferential block voting is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "Oklahoma" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = OklahomaMethod.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Oklahoma is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "SPAV" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = SequentialProportionalApprovalVoting.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method SPAV is not implemented yet.")

          r.writeWinners(winnersfile)
        }

        case "PAV" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          var r = ProportionalApprovalVoting.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method PAV is not implemented yet.")
          r.writeWinners(winnersfile)
        }

      case "SAV" => {
        val election = PreferencesParser.read(c.directory + electionFile)
        var r = SatisfactionApprovalVoting.runScrutiny(Election.weightedElectionToACTElection(election), candidates_in_order, c.nvacancies.toInt)
        println(" Scrutiny table for method SAV is not implemented yet.")
        r.writeWinners(winnersfile)
      }
        case "SMC" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          parameters match {
            case Some(param) => {
              var r = SMCMethod.runScrutiny(election, candidates_in_order, param, c.nvacancies.toInt)
              println("Scrutiny table for method SMC is not implemented yet.")
              r.writeWinners(winnersfile)
            }
            case None => println("\n\nPlease provide the comparison order to execute this voting method\n\n")
          }
        }

        case "RankedPairs" => {
          val election = PreferencesParserWithIndifference.read(c.directory + electionFile)
          var r = RankedPairsMethod.runScrutiny(election, candidates_in_order, c.nvacancies.toInt)
          println(" Scrutiny table for method Ranked Pairs is not implemented yet.")
          r.writeWinners(winnersfile)
        }

        case "BipartisanSet" => {
          val election = PreferencesParser.read(c.directory + electionFile)
          parameters match {
            case Some(param) => {
              var r = BipartisanSet.runScrutiny(election, candidates_in_order, param)
              println("Scrutiny table for Bipartisan Set is not implemented yet.")
              r.writeWinners(winnersfile)
            }
            case None => println("Please provide probability distribution to compute bipartisan set")
          }
        }

        case "Test" => {
          Test.testSDResolution
        }
        case "" => println("Please, specify which algorithm should be used. Only option -m EVACS is currently stable.")
      }
    }

    def methodParameters(c: Config): Option[Parameters] = {
      c.parameterFile match {
        case Some(fileName) => {
          Option(ParameterParser.parse(c.directory + fileName))
        }
        case None => None
      }
    }

    parser.parse(args, Config()) map { c =>

      c.ballotsfile match {
        case Some(filename) => { // ONLY ONE FILE IS ANALYSED
          val candidates = CandidatesParser.read(c.directory + c.candidatesfile)
          println("Candidates: " + candidates)
          val winnersfile = c.directory + "winners/" + "Winners_" + c.method + "_InputFile_" + filename
          val reportfile = c.directory + "reports/" + "Report_" + c.method + "_InputFile_" + filename
          callMethod(c, filename, winnersfile, reportfile, candidates, methodParameters(c))
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
            callMethod(c, filename, winnersfile, reportfile, candidates, methodParameters(c))
          }
        }
      }
    }
  }
}

