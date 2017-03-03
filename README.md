Agora is a library of data structures and algorithms for counting votes in elections.


Currently the following methods are implemented:  

1) EVACSMethod (imitation of ACT's EVACS system)  
2) SimpleSTVMetod (a simple STV counting algorithm)  
3) Egalitarian method  
4) Majority rule method   


Remark: When EVACSMethod is run on real data the computation is slow because of ACTFractionLoss

## Input Files

### Ballots

```
id1 weight preference_1^1 > ... > preference_1^{N_1}  
id2 weight preference_2^1 > ... > preference_2^{N_2}
...  
idK weight preference_K^1 > ... > preference_K^{N_K}  
```
Weight has to be a rational number and it is initially normally equal to 1/1.  

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

* preference data of the following elections: ACT Legislative Assembly 2004, ACT Legislative Assembly 2008, ACT Legislative Assembly 2012 - all are formatted for Agora

* some simple examples of elections (for testing)

## Running the code 

java -jar countvotes-assembly-1.1.jar parameters

OR

sbt -J-Xmx12G -J-Xms12G  

run -d /directory/ [-b ballots.txt] -c candidates.txt -m method -v num_of_vacancies [-k num_of_candidates] [-t table]

Example:  

run -d /Users/lebedka/Code/Voting/Agora/files/Examples/ -b 02-example.txt -c 02-candidates.txt -m Majority -v 1

where

* method is either EVACS, Simple, Egalitarian or Majority  

* candidates.txt is the file containing all candidates. The tally will be printed in the order as the candidates appear in this file.

* table is either ACT (a simulation of ACT's scrutiny tables) or Concise. Concise is default.

Note: If -b is omitted, all files in -d are analysed.

