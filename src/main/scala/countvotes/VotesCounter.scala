
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
      
     var r:  List[(Candidate,Rational)] = List()
     
     c.algorithm match {
       case "EVACS" =>  {
         r = EVACSMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt) 
       }
       case "Simple" =>  {
         r = SimpleSTVMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt) 
       }
       case "Test" =>  {
         Test.testSDResolution
       }
       case "" =>  println("Please, specify which algorithm should be used. Only options -a EVACS and -a Simple are currently available.")
     }
    
     println("Winners: " + r)
        
     val outputfile = c.directory + "WinnersByAlgorithm_" + c.algorithm + "_InputFile_" + c.file
     val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputfile)))      
     //writer.write(result.getWinners.toString())
     var sw = ""  
     for ( w <- r){
         sw = sw + w._1 + ": " + w._2.numerator/w._2.denominator + "\n"
       }
      writer.write(sw)
     writer.close()
        
     val routputfile = c.directory + "Report_" + c.algorithm + "_InputFile_" + c.file
     //writeDistributionOfPreferences(routputfile, report)
     
   }
  }

}