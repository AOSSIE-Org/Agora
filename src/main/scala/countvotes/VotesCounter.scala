
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

object Main {

  case class Config(directory: String = "",
                    file: String = "",
                    algorithm: String = "",
                    nvacancies: String = "")

  val parser = new scopt.OptionParser[Config]("compress"){
    head("\nCommand Line Interface for Electronic Vote Counting\n\n  ")

    note("""The following arguments have to be provided:""" + "\n" +
        """ -d -f -a -n""" + "\n \n"
    )

    opt[String]('d', "directory") unbounded() action { (v, c) =>
      c.copy(directory = v)
    } text("set working directory to <dir>\n") valueName("<dir>")

    opt[String]('f', "file") action { (v, c) =>
      c.copy(file = v)
    } text("use preferences listed in <file>\n") valueName("<file>")

    opt[String]('a', "algorithm") action { (v, c) =>
      c.copy(algorithm = v)
    } text("use vote counting algorithm of territory/state  <alg>\n") valueName("<alg>")

    opt[String]('n', "nvacancies") action { (v, c) =>
      c.copy(nvacancies = v)
    } text("set number of vacancies  <num>\n") valueName("<num>")
  }


  def main(args: Array[String]): Unit = {

   parser.parse(args, Config()) map { c =>

     println("Parsing started...")
     val election =  PreferencesParser.read(c.directory + c.file)
     println("Parsing finished.")

     //var r:  List[(Candidate,Rational)] = List()

     val winnersfile = c.directory + "WinnersByAlgorithm_" + c.algorithm + "_InputFile_" + c.file
     val reportfile = c.directory + "Report_" + c.algorithm + "_InputFile_" + c.file

     c.algorithm match {
       case "EVACS" =>  {
        var r = EVACSMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt)
        r.writeDistributionOfPreferences(reportfile)
        println("The scrutiny was recorded to " + reportfile)
        r.writeWinners(winnersfile)
        println("The winners were recorded to " + winnersfile)
       }
       case "EVACSnoLP" =>  {
        var r = EVACSnoLPMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt)
        r.writeDistributionOfPreferences(reportfile)
        r.writeWinners(winnersfile)
       }
       case "Simple" =>  {
        var r = SimpleSTVMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt)
        println(" Scrutiny table for method Simple is not implemented yet.")
        r.writeWinners(winnersfile)
       }
       case "Egalitarian" =>  {
        var r = EgalitarianImplementation.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt)
        println(" Scrutiny table for method Egalitarian is not implemented yet.")
        r.writeWinners(winnersfile)
       }
       case "Test" =>  {
         Test.testSDResolution
       }
       case "" =>  println("Please, specify which algorithm should be used. Only option -a EVACS is currently available.")
     }


   }
  }

}
