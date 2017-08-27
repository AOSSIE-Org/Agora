#Summary
I have enriched the AGORA voting library with several state-of-the-art voting methods
along with their unit tests and performance analysis framework. In addition to that, several analysis
mechanisms has also been added such as preference analysis of voters and stability analysis of voting
methods. AGORA library now also supports preference ballots with indifference, ranks and scores.
For some voting methods which require specific data for processing, a new json based architecture
is added for data passing.


#Integrating Scalameter

For unit testing and performance benchmark, Scalameter has been used. In order to validate the voting method
implementation, a unit test is written which uses the example from the source of the voting method(Wikipedia).
For microbenchmarking, random elections of voters sizes 10000, 15000 and 20000 are created and the algorithm
is executed on these generated election several times. At last, final offline regression report is generated
in the target folder which shows the graph of method's performance against the generated elections. One existing
issue with the scalameter OfflineRegressionReport is that, the HTML reporter does not properly read the report
directories(https://github.com/scalameter/scalameter/issues/159) due to which it fails to create two separate
reports for memory regression and runtime regression. One problem I faced with this is, the build was failing
on the CI build server of the AGORA repository because it was executing all the regression methods and it was
taking more than 70 minutes. In order to fix that, I had to change the configuration in .gitlab-ci.yml file so
that it executes only unit tests but not regression tests. This whole testing framework currently supports only
those voting methods which uses WeightedBallot but future work needs to be done so that it supports all different
kind of ballots Agora offers to all the voting methods.



#unified framework for passing algorithm specific information to Agora

Almost all the algorithms that Agora offers require an election file and set of candidates. Additionally, there
are some voting methods like Sequential Majority Comparison which requires a comparison order of candidates to
elect a winner, bi-partisan set requires a probability distribution over candidates, kelly and fishburn’s extension
require two set of candidates for comparison, etc. Since the nature of the data required by this algorithms were
fundamentally different, a new json based framework is added which facilitates passing any algorithm specific data
to any voting methods. While executing any of these method an extra command line parameter is added which provides
the json data file which contains method specific data. Moreover, this parameter is optional which means if the
voting method does not need any specific data then this parameter doesn’t need to be passed. Internally, all the
data is mapped to a single case class which is  then further passed to a voting method for processing. In addition
to that, the json data file only need to contain the fields/data which is relevant to the voting method which is
really cool otherwise we would have to add all the unnecessary key data fields from the other voting methods.



# state-of-the-art voting methods

Smith Set(a.k.a Topcycle/GETCHA) - This is undoubtedly one of the most natural condorcet extension set method.
The current implementation uses the wikipedia article description which uses O(n3) Floyd warshall algorithm.
This can be further improved to linear time implementation which extends the set using Copeland Winner.

Uncovered Set - This is again one of the important condorcet extension tournament solution which satisfies
Pareto-optimality on candidates over voters preferences. It’s implementation currently uses Matrix multiplication
algorithm to compute the set. In future more unit test can be included to cover the corner cases.

Bipartisan Set - This is relatively one of the advanced Condorcet extension that has been added to the Agora
library. It is based on the paper from Laffond et al., 1993; Fisher & Ryan, 1995 => Every tournament admits a
unique balanced probability distribution. The existence of this balanced probability distribution is guaranteed
by Von Neumann’s Minimax Theorem(1928). The current implementation in Agora takes this probability distribution
using json file and computes the set but in future this probability distribution can be calculated itself via
linear programming.

Kelly’s extension: Agora now offers set extension methods to define strategy-proofness. One such set extension
method is Kelly’s extension. Its current implementation in Agora takes the sets for comparison using json file.
In future, more unit test can be added to test cover cases and also its performance regression can be done.

Fishburn’s extension: Another set extension method which takes O(n2) time to compute the winner set. In future,
more unit test cases can be added to test cover cases and also its performance regression can be done.


# traditional Voting methods:

Many traditional voting algorithms have been added such as Borda, Coomb, Copeland, InstantRunoff2Round,
KemenyYoungMethod, MinimaxCondorcetMethod, Range Voting, Schulze etc. They all have been implemented using
algorithm description given on their wikipedia article. Most of them are really straightforward and require
either elimination of candidates or founding the candidate with max score etc. Their unit tests and regression
tests have also been added to the Agora. For testing Random Ballot method a fixed seed is used to make sure we
get the same results each time however this fixed seed is not used while calculating the actual winner in the
election. Another interesting method is Sequential majority comparison which needs specific order of candidates
to compute the winner. This order of candidate are provided through json data file and the method neatly uses
scala’s foldLeft operation to compute the winner. For more trickier voting methods like Ranked Pairs, a graph
library has been added to the Agora. This method has some ambiguities in the way it resolves ties while sorting
candidates and in future, more unit tests can be added which cover the corner cases for this ties ambiguities.
Another challenging implementation was of Dodgson Voting method whose brute force approach uses flip vector idea.
In future, improved heuristics can be added to efficiently compute the Dodgson winner.


#preference analysis

Many times it’s interesting to analyse voters preferences to find some patterns or to predict something. For
example, if the voters preferences are single-peaked,single-single caved or value restricted then there is a
good chance that condorcet cycle does not exist in the preferences. Agora now offers 3 methods for preference
analysis which are Single-Peaked, Single-Caved and Value restricted by Amartya Sen. I had a lot of difficulty
implementing Single Peaked preferences as it involves a very sophisticated recursion with lots of corner cases.
In another analyser(Value restricted), I had a challenging task to implement the functionality using Lazy views
from scala which was really confusing in the beginning but was really worth the effort at the end. In future,
performance and memory footprint regression can be done for all of these methods to check their performance.


#stability analysis

At the end of the project it was important for us to calculate the stability of the voting algorithms, so we
added a new stability analysis procedure which uses our own algorithm to calculate the stabilities of the voting
methods. The algorithm used is described in https://gitlab.com/aossie/Agora/issues/75. Currently, the analysis is
done only for limited voting methods which uses only WeightedBallots but in future it can be extended to included
other voting methods too which uses other ballots.



#Indifference Ballot and class Ballot hierarchies

Preferences with Indifference ballot was a tricky one because it just does not parses the candidates and creates
the list but also calculates the rank of each candidate based on the symbols “=/>”. Implementation of it involves
combining many other different parsers functionalities that scala language offers. We also faced a lot of challenges
when dealing with ballot hierarchies. Initially, Agora supported only “>” format ballots but when more ballots are
added of different formats then there was a need to refactor class ballot hierarchies. We tried several approaches
to solve this problem including using implicit conversion of ballots but it did not work out very well. In the end,
we chose to call specific parsers for the specific voting methods which returns the expected ballot elections. We
also created two different classes for RankedWeightedBallot and ScoreWeightedBallot even though they have the same
structure but they differ in their semantic meaning.


#project repository

https://gitlab.com/aossie/Agora


#hosted executable

TODO


#GSOC-2017 TAG

TODO


#Merge Requests

MERGED
·      [MR6: Borda Method] https://gitlab.com/aossie/Agora/merge_requests/6
·      [MR14: Nanson Method] https://gitlab.com/aossie/Agora/merge_requests/14
·      [MR15: kemeny Young Method] https://gitlab.com/aossie/Agora/merge_requests/14
·      [MR23: Coomb Method] https://gitlab.com/aossie/Agora/merge_requests/23
·      [MR26: Minimax Condorcet] https://gitlab.com/aossie/Agora/merge_requests/26
·      [MR29: Logging Library] https://gitlab.com/aossie/Agora/merge_requests/29
·      [MR30: Added Scalameter] https://gitlab.com/aossie/Agora/merge_requests/30
·      [MR32: Sbt Configuration] https://gitlab.com/aossie/Agora/merge_requests/32
·      [MR35: RunnOff2Round] https://gitlab.com/aossie/Agora/merge_requests/35
·      [MR37: Scalameter Refactor] https://gitlab.com/aossie/Agora/merge_requests/37
·      [MR38: RandomBallot] https://gitlab.com/aossie/Agora/merge_requests/38
·      [MR45: Preference Analysis] https://gitlab.com/aossie/Agora/merge_requests/45
·      [MR47: Added Test for Borda] https://gitlab.com/aossie/Agora/merge_requests/47
·      [MR55: Smith Set] https://gitlab.com/aossie/Agora/merge_requests/55
·      [MR57: Uncovered Set] https://gitlab.com/aossie/Agora/merge_requests/57
·      [MR61: Single Peak] https://gitlab.com/aossie/Agora/merge_requests/61
·      [MR62: Copeland] https://gitlab.com/aossie/Agora/merge_requests/62
·      [MR63: Compilation Error] https://gitlab.com/aossie/Agora/merge_requests/63
·      [MR64: Json Framework] https://gitlab.com/aossie/Agora/merge_requests/64
·      [MR65: Value Restriction] https://gitlab.com/aossie/Agora/merge_requests/65
·      [MR66: Indifference Ballots] https://gitlab.com/aossie/Agora/merge_requests/66
·      [MR67: Bipartisan Set] https://gitlab.com/aossie/Agora/merge_requests/67
·      [MR71: Range Voting] https://gitlab.com/aossie/Agora/merge_requests/71
·  	[MR72: Schulze Algorithm] https://gitlab.com/aossie/Agora/merge_requests/72
·      [MR73: Parameter fix] https://gitlab.com/aossie/Agora/merge_requests/73

OPEN
·      [MR59: Dodgson] https://gitlab.com/aossie/Agora/merge_requests/59
·      [MR69: Fishburn’s extension] https://gitlab.com/aossie/Agora/merge_requests/69
·      [MR70: Stability analysis] https://gitlab.com/aossie/Agora/merge_requests/70
·      [MR72: Kelly extension] https://gitlab.com/aossie/Agora/merge_requests/74
