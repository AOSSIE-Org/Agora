
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
                    file: Option[String] = None, 
                    method: String = "",
                    nvacancies: String = "",
                    order: String = "")   
  
  val parser = new scopt.OptionParser[Config]("compress"){
    head("\nCommand Line Interface for Electronic Vote Counting\n\n  ")        
    
    note("""The following arguments have to be provided:""" + "\n" + 
        """ -d -f -m -n -o""" + "\n \n"
    )  
        
    opt[String]('d', "directory") unbounded() action { (v, c) => 
      c.copy(directory = v)
    } text("set working directory to <dir>\n") valueName("<dir>")
    
    opt[String]('f', "file") action { (v, c) =>
      c.copy(file = Some(v)) 
    } text("use preferences listed in <file>\n") valueName("<file>")
    
    opt[String]('m', "method") action { (v, c) =>
      c.copy(method = v) 
    } text("use vote counting method of territory/state  <met>\n") valueName("<met>")
    
    opt[String]('n', "nvacancies") action { (v, c) =>
      c.copy(nvacancies = v) 
    } text("set number of vacancies  <num>\n") valueName("<num>")
    
    opt[String]('o', "order") action { (v, c) =>
      c.copy(order = v) 
    } text("set order in which the candidates appear in output tables <ord>\n") valueName("<ord>")
  }

  
  def main(args: Array[String]): Unit = {
    
    
    def callMethod(c: Config, election: List[WeightedBallot],  winnersfile:String, reportfile: String, order:  List[Candidate]) = {
      c.method match {
               case "EVACS" =>  {
                 var r = EVACSMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt) 
                 if (order.nonEmpty) r.writeDistributionOfPreferences(reportfile,Some(order)) else  r.writeDistributionOfPreferences(reportfile,None)
                 println("The scrutiny was recorded to " + reportfile)
                 r.writeWinners(winnersfile)
                 println("The winners were recorded to " + winnersfile)
               }
               case "EVACSnoLP" =>  {
                 var r = EVACSnoLPMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt) 
                 if (order.nonEmpty)  r.writeDistributionOfPreferences(reportfile,Some(order)) else  r.writeDistributionOfPreferences(reportfile,None)
                 r.writeWinners(winnersfile)
               }
               case "EVACSDWD" =>  {
                  var r = EVACSDelayedWDMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt) 
                  if (order.nonEmpty) r.writeDistributionOfPreferences(reportfile,Some(order)) else r.writeDistributionOfPreferences(reportfile,None)
                  r.writeWinners(winnersfile)
               }
               case "Simple" =>  {
                  var r = SimpleSTVMethod.runScrutiny(Election.weightedElectionToACTElection(election), c.nvacancies.toInt) 
                  println(" Scrutiny table for method Simple is not implemented yet.")
                  r.writeWinners(winnersfile)
               }
               case "Test" =>  {
                  Test.testSDResolution
               }
               case "" =>  println("Please, specify which algorithm should be used. Only option -a EVACS is currently available.")
           }
    }
    
    
   parser.parse(args, Config()) map { c =>
        
     var order: List[Candidate] = Nil
     c.order match {
       case "ACTGinninderra2004" =>     
         order = List(new Candidate("Ben O'CALLAGHAN"), 
                      new Candidate("Meredith HUNTER"),
                      new Candidate("Adam PORTER"),
                      new Candidate("Rose PAPPALARDO"),
                      new Candidate("Roberta WOOD"), 
                      new Candidate("Roslyn DUNDAS"), 
                      new Candidate("Harold HIRD"), 
                      new Candidate("Julie-Anne PAPATHANASIOU"), 
                      new Candidate("Darcy HENRY"), 
                      new Candidate("John E. GORMAN"),
                      new Candidate("Bill STEFANIAK"), 
                      new Candidate("Bob SOBEY"), 
                      new Candidate("Briant CLARK"), 
                      new Candidate("Ilona FRASER"), 
                      new Candidate("Vicki DUNNE"), 
                      new Candidate("Anne MOORE"), 
                      new Candidate("Mike O'SHAUGHNESSY"), 
                      new Candidate("Jon STANHOPE"), 
                      new Candidate("Mary PORTER"), 
                      new Candidate("Ross MAXWELL"), 
                      new Candidate("Susan McCARTHY"), 
                      new Candidate("Wayne BERRY"), 
                      new Candidate("John SIMSONS"))
       case "ACTBrindabella2004" =>
         order = List(new Candidate("Erol Francis BYRNE"), 
                      new Candidate("Thelma JANES"),
                      new Candidate("Graham JENSEN"),
                      new Candidate("Kathryn KELLY"),
                      new Candidate("Brendan SMYTH"), 
                      new Candidate("Karen SCHILLING"), 
                      new Candidate("Megan PURCELL"), 
                      new Candidate("Steve DOSZPOT"), 
                      new Candidate("Steve PRATT"), 
                      new Candidate("Marc EMERSON"),
                      new Candidate("Rowena BEW"), 
                      new Candidate("David GARRETT"), 
                      new Candidate("Matthew HARDING"), 
                      new Candidate("John HARGREAVES"), 
                      new Candidate("Karin MacDONALD"), 
                      new Candidate("Mick GENTLEMAN"), 
                      new Candidate("Paschal LEAHY"), 
                      new Candidate("Rebecca LOGUE"), 
                      new Candidate("Burl DOBLE"), 
                      new Candidate("Lance MUIR"), 
                      new Candidate("Stephanie ELLIOTT"))
       case "ACTGinninderra2004" =>
         order = List(new Candidate("Fred LEFTWICH"), 
                      new Candidate("Robert ROSE"),
                      new Candidate("John HUMPHREYS"),
                      new Candidate("Melanie SUTCLIFFE"),
                      new Candidate("John FARRELL"),
                      new Candidate("Robert FEARN"),
                      new Candidate("Adina CIRSON"),
                      new Candidate("Andrew BARR"),
                      new Candidate("Katy GALLAGHER"),
                      new Candidate("Kim SATTLER"),
                      new Candidate("Mike HETTINGER"),
                      new Candidate("Simon CORBELL"),
                      new Candidate("Ted QUINLAN"),
                      new Candidate("David KIBBEY"),
                      new Candidate("Gordon SCOTT"),
                      new Candidate("Jacqui BURKE"),
                      new Candidate("Lucille BAILIE"),
                      new Candidate("Richard MULCAHY"),
                      new Candidate("Ron FORRESTER"),
                      new Candidate("Zed SESELJA"),
                      new Candidate("Amanda BRESNAN"),
                      new Candidate("Charlie PAHLMAN"),
                      new Candidate("Deb FOSKEY"),
                      new Candidate("Jo McKINLEY"),
                      new Candidate("Simone GRAY"),
                      new Candidate("Helen CROSS"),
                      new Candidate("Renee STRAMANDINOLI"),
                      new Candidate("Jonathon REYNOLDS"),
                      new Candidate("Nancy-Louise McCULLOUGH"),
                      new Candidate("Ken HELM"),
                      new Candidate("Kurt KENNEDY"),
                      new Candidate("Luke GARNER"),
                      new Candidate("Tony FARRELL"))
       case "ACTBrindabella2012" =>
         order = List(new Candidate("WALL Andrew"), 
                      new Candidate("SMYTH Brendan"), 
                      new Candidate("LAWDER Nicole"), 
                      new Candidate("JEFFERY Val"), 
                      new Candidate("SESELJA Zed"), 
                      new Candidate("BRESNAN Amanda"), 
                      new Candidate("MURPHY Ben"), 
                      new Candidate("DAVIS Johnathan"), 
                      new Candidate("BURCH Joy"), 
                      new Candidate("MAFTOUM Karl"), 
                      new Candidate("GENTLEMAN Mick"), 
                      new Candidate("KINNIBURGH Mike"), 
                      new Candidate("CODY Rebecca"), 
                      new Candidate("HENSCHKE Adam"), 
                      new Candidate("ERWOOD Mark"), 
                      new Candidate("DOBLE Burl"), 
                      new Candidate("JONES-ELLIS Kieran"),
                      new Candidate("PEARCE Calvin"),
                      new Candidate("GIBBONS Mark"),
                      new Candidate("LINDFIELD Michael"))
       case _ =>
     }
     
     
     c.file match {
       case Some(filename) => { // ONLY ONE FILE HAS TO BE ANALYSED
            val election =  PreferencesParser.read(c.directory + filename)
            val winnersfile = c.directory + "/winners/" + "Winners_" + c.method + "_InputFile_" + filename
            val reportfile = c.directory + "/reports/" + "Report_" + c.method + "_InputFile_" + filename 
            callMethod(c, election, winnersfile, reportfile, order) 
       }
       case None => {  // ALL FILES IN THE DIRECTORY HAVE TO BE ANALYSED
        val files = new java.io.File(c.directory).listFiles.filter(_.getName.endsWith(".txt"))
        for (file <- files){
          val filename = file.getName
          println("/n" + file.getName + "/n")          
          val election =  PreferencesParser.read(c.directory + filename)
          val winnersfile = c.directory + "/winners/" + "Winners_" + c.method + "_InputFile_" + filename
          val reportfile = c.directory + "/reports/" + "Report_" + c.method + "_InputFile_" + filename 
          callMethod(c, election, winnersfile, reportfile, order) 
        }
       }
     }
   }
  }

}