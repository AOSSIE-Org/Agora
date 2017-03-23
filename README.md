
[![build status](https://gitlab.com/aossie/Agora/badges/master/build.svg)](https://gitlab.com/aossie/Agora/commits/master)
[![coverage report](https://gitlab.com/aossie/Agora/badges/master/coverage.svg)](https://gitlab.com/aossie/Agora/commits/master)

Agora is a library of data structures and algorithms for counting votes in elections.

Currently the following methods are implemented:  

* `EVACS` - an imitation of [eVACS](http://www.elections.act.gov.au/elections_and_voting/electronic_voting_and_counting), the STV system used for counting votes in Australian Capital Territory. The [ACT Electoral Act](http://www.legislation.act.gov.au/a/1992-71/default.asp) specifies the STV method used in ACT.
* `Simple` - a simple STV counting algorithm.
* `Egalitarian`  
* `Majority` 
* `Borda`
* `Approval`
* `Nanson`
* `Kemeny-Young` 

Remark: When EVACSMethod is run on real data the computation is slow because of ACTFractionLoss

## Input Files

### Ballots

```
id1 weight preference_1^1 > ... > preference_1^{N_1}  
id2 weight preference_2^1 > ... > preference_2^{N_2}
...  
idK weight preference_K^1 > ... > preference_K^{N_K}  
```
Weight has to be a rational number and its initial value is normally equal to 1/1.  

Example:  

```
1 1/1 A  
2 1/1 A > B  
3 1/1 B > A > C  
4 1/1 C  
```

### Candidates

```
candidate1
candidate1
...
candidateN
```

Example:

```
A
B
C
```



### Preference Data

Folder "files" contains 

* Preference data of the following elections: ACT Legislative Assembly 2004, ACT Legislative Assembly 2008, ACT Legislative Assembly 2012 - all are formatted for Agora. The originals can be found [on the page of the ACT Electoral Commission](http://www.elections.act.gov.au/elections_and_voting/past_act_legislative_assembly_elections). 

* Some simple examples of elections (for testing)

## Usage Instructions 

You must have [SBT](http://www.scala-sbt.org/release/docs/Setup.html) (version >= 0.13) installed. SBT automatically downloads all compilers and libraries on which  Agora depends.

To start SBT, go to Agora's home folder using the terminal and run:
```
$ sbt
```

`EVACS` (an imitation of ACT's eVACS STV system) requires more memory, thus run:
```
$ sbt -J-Xmx12G -J-Xms12G  
```
(you may change the values after Xmx and Xms to suit your needs)

Then you can run Agora within SBT's command line. The following command runs the Majority method on preferences given by file `02-example.txt` and candidates enumerated in file `02-candidates.txt` looking for `1` winner.

```
run -d files/Examples/ -b 02-example.txt -c 02-candidates.txt -m Majority -v 1
```
The general command to run Agora is as follows:

```
run -d /directory/ [-b ballots.txt] -c candidates.txt -m method -v num_of_vacancies [-k num_of_candidates] [-t table]
```

where

* method is `EVACS`, `Simple`, `Egalitarian`, `Majority`, `Approval`, `Borda`, `Kemeny-Young` or `Nanson`

* `candidates.txt` is the file containing all candidates. The tally will be printed in the order as the candidates appear in this file.

* `table` is either `ACT` (a simulation of ACT's scrutiny tables) or `Concise`. Concise is default.

Note: If -b is omitted, all files in -d are analysed.

