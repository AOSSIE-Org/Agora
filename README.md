EVoting implements some STV algorithms in a recursive way.  
Common components are implemented as traits.  


Currently the following STV methods are implemented:  

1) EVACSMethod (imitation of ACT's EVACS system)  
2) SimpleSTVMetod (a simple STV counting algorithm)  

-----------------------------------------------------------------
REMARKS:
-----------------------------------------------------------------

When EVACSMethod is run on real data the computation is slow because of ACTFractionLoss

-----------------------------------------------------------------
INPUT FILE FORMAT:
-----------------------------------------------------------------

id1 weight preference1 > ... > preferenceN  
id2 weight preference1 > ... > preferenceM  
id3 weight preference1 > ... > preferenceK  

weight has to be a rational number.  
In the beginning of scrutiny weight is normally equal to 1/1.  

Example:  

1 1/1 A  
2 1/1 A > B  
3 1/1 B > A > C  
4 1/1 C  


-----------------------------------------------------------------   
Real Preference Data
-----------------------------------------------------------------

Folder "files" contains preference data of the following elections in format accepted by EVoting.

1) ACT Legislative Assembly 2004  
2) ACT Legislative Assembly 2008  
3) ACT Legislative Assembly 2012  

-----------------------------------------------------------------   
Examples of TERMINAL COMMANDS to run the code:  
-----------------------------------------------------------------  

sbt -J-Xmx12G -J-Xms12G  

run -d /directory/ [-b ballots.txt] -c candidates.txt -m method -v num_of_vacancies [-k num_of_candidates] [-t table]


run -d /home/users/u1017108/Documents/PreferenceData/ACT/2004/ -b Preferences_ACT2004_Ginninderra.txt -c Candidates_ACT_2004_Ginninderra.txt -m EVACS -v 5 -t Concise

-----------------------------------------------------------------

java -jar countvotes-assembly-1.1.jar parameters

where

method is either EVACS or Simple  
candidates is the file containing all candidates. The tally will be printed in the order as the candidates appear in this file.

table is either ACT or Concise. Concise is default.

If -f is ommited, all files in -d are analyzed.

-----------------------------------------------------------------
TODO:
-----------------------------------------------------------------

GENERAL:
 
1) Optimize filtering pending winners (can be done once outside functions exclude and distributesurplus)

BUG:
Candidates that do not appear as a first preference in one of the ballots never win by algorithm Simple

