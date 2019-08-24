
# Summary

I have worked on the Agora voting library during my summers with AOSSIE, in which my work involved implementing new voting algorithms in Scala along with their unit tests. I also implemented some new quotas and some new methods of breaking ties. 

# Project repository

[AOSSIE](https://gitlab.com/aossie/Agora)


# GSOC-2019 TAG

TODO

# Merge Requests

## My Merged requests

1. [Implemeted the Imperiali Quota](https://gitlab.com/aossie/Agora/merge_requests/87)

2. [Implemented Hagenbach Bischoff Quota](https://gitlab.com/aossie/Agora/merge_requests/89)

3. [Implemented D'hondt Quotient Quota](https://gitlab.com/aossie/Agora/merge_requests/88)

4. [Implemented Webster-Sainte-Lague Quota](https://gitlab.com/aossie/Agora/merge_requests/91)

5. [Implemented Huntington-Hill quota](https://gitlab.com/aossie/Agora/merge_requests/92)

6. [Implement DMP ballot](https://gitlab.com/aossie/Agora/merge_requests/94)

7. [Implemented Bucklin method](https://gitlab.com/aossie/Agora/merge_requests/96)
    - Bucklin voting is a class of voting methods that can be used for single-member and multi-member districts. Voters are allowed rank preference ballots (first, second, third, etc.). First choice votes are first counted. If one candidate has a majority, that candidate wins. Otherwise the second choices are added to the first choices. Again, if a candidate with a majority vote is found, the winner is the candidate with the most votes accumulated. Lower rankings are added as needed.

8. [Implemented veto method](https://gitlab.com/aossie/Agora/merge_requests/100)
    - In the Veto rule each candidate with the exception of the candidate at the last position gains one point per vote. As for Plurality the candidates with maximum score are the winners.

## Open

9. [Implemented super majority method](https://gitlab.com/aossie/Agora/merge_requests/95)
    - A majority vote, or more than half the votes cast, is a common voting basis. There is already an implementation of this algorithm in Agora. Instead of the basis of a majority, a supermajority can be specified using any fraction or percentage which is greater than one-half (i.e., 50%). It can also be called a qualified majority. Common supermajorities include three fifths (60%), two thirds (67%), and three quarters (75%). This can be easily added by modifying the runScrutiny function of MajorityRuleMethod.

10. [Implemented scottish stv](https://gitlab.com/aossie/Agora/merge_requests/97)
    - Scotland enacted these rules for local elections in 2007. This is a straightforward implementation of STV and recommended to organizations using STV for the first time. Scottish STV is just a variant of the STV method implemented in Agora library

11. [Implemented Cambridge STV method](https://gitlab.com/aossie/Agora/merge_requests/98)
    - The Cambridge STV is yet another variation of STV that was introduced by Cambridge to elect its city council.

12. [Implemented San Francisco RCV method](https://gitlab.com/aossie/Agora/merge_requests/99)
    - The city of San Francisco uses the San Francisco RCV rules for city elections. San Francisco enacted IRV in 2002, its first election with IRV was in 2004, and it has been used annually since then.

13. [Implemented Black voting method](https://gitlab.com/aossie/Agora/merge_requests/101)
    - The Black rule is a 'combined' voting rule that returns the Condorcet winner, if such a winner exists, or otherwise the Borda winner(s).

14. [Implemented instant runoff voting an equivalent of Oakland RCV](https://gitlab.com/aossie/Agora/merge_requests/104)
    - This method implements IRV as used by the cities of Oakland, San Leandro, and Berkeley. All three cities had their first IRV elections in 2010.

15. [Added prior round exclusion tie resolution](https://gitlab.com/aossie/Agora/merge_requests/105)
    - This is a tie breaking technique that looks at the prior round to determine which candidate amongst the tied candidate had a better score and hence is made the winner.

16. [Implemented later preference exclusion tie resolution](https://gitlab.com/aossie/Agora/merge_requests/106)
    - This is a tie breaking method which verifies which of the tied candidate has highest second choice score of all voters. This tie breaking technique works only for elections in which multiple candidates are selected by the voters (preferential ballots) and not for single choice ballots.

17. [Added Documentation about GSoC 2019 work](https://gitlab.com/aossie/Agora/merge_requests/107)


# Addition of various voting algorithms

I have added various voting algorithms that I found out on Wikipedia and Opavote. They involve Super Majority, Bucklin Vote, Scottish STV, Cambridge STV, San Francisco RCV, Oakland RCV, Veto etc.
 
# Addition of new method of breaking ties

I have added some new methods of breaking ties which are particularly useful for some STV algorithms. These methods include Prior-round tie breaking and Later-preference tie breaking methods
 
# Addition of algorithms to calculate quotas

Some new algorithms used to calculate quotas have implemented such as Hagenbach-Bischoff, D’Hondt-Quotient, Imperiali-quota, Webster-Sainte-Lague etc. Some of which are used by the new vote counting methods that have been added. 

# Addition of new ballot 

A new ballot type has been added to the library which is known as DMP (Dual member proportional representation) ballot. This will be used in conjunction with the Dual member proportional representation method.

# Things left to tackle

The Dual member proportional representation method has not yet been implemented and scrutiny tables for the rest of the algorithms are not created yet which will need some work and discussion with mentors and project admins.

I would like to thank my mentors at AOSSIE for being highly supportive and co-operative with me during the coding period. With their constant support I was able to learn a lot during these 3 months of coding. 
