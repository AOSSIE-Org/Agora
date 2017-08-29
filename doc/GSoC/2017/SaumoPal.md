
# Summary

I have worked on the Agora voting library during my summers with AOSSIE, in which my work involved implementing various new voting algorithms in Scala along with their unit tests. For covering a wide variety of voting algorithms, modifying the existing ballot and parser methods to support rank and score of voters’ choices was necessary, which was taken up during the start of the coding period. Also, addition of new algorithms for calculating new forms of quotas has also been undertaken.

# Project repository

[AOSSIE](https://gitlab.com/aossie/Agora)

# Hosted executable

https://zenodo.org/record/852440#.WaTmkXWGNn1

# GSOC-2017 TAG

TODO

# Merge Requests

## My Merged requests

* [MR5: Used scalastyle to check adherence to coding style conventions][https://gitlab.com/aossie/Agora/merge_requests/5](https://gitlab.com/aossie/Agora/merge_requests/5)

* [MR13: Added Approval Rule][https://gitlab.com/aossie/Agora/merge_requests/13](https://gitlab.com/aossie/Agora/merge_requests/13)

* [MR25: Added ContingentMethod with Contingent Test][https://gitlab.com/aossie/Agora/merge_requests/25](https://gitlab.com/aossie/Agora/merge_requests/25)

* [MR31: Added new parser and one more example][https://gitlab.com/aossie/Agora/merge_requests/31](https://gitlab.com/aossie/Agora/merge_requests/31)

* [MR33: Added ScoredBallot and related functions][https://gitlab.com/aossie/Agora/merge_requests/33](https://gitlab.com/aossie/Agora/merge_requests/33)

* [MR40: Added preferential block voting with ranked ballot][https://gitlab.com/aossie/Agora/merge_requests/40](https://gitlab.com/aossie/Agora/merge_requests/40)

* [MR43: Instant Exhaustive Ballot][https://gitlab.com/aossie/Agora/merge_requests/43](https://gitlab.com/aossie/Agora/merge_requests/43)

* [MR44: Added exhaustive ballot with drop off rule][https://gitlab.com/aossie/Agora/merge_requests/44](https://gitlab.com/aossie/Agora/merge_requests/44)

* [MR46: Added Nanson without baldwin rule][https://gitlab.com/aossie/Agora/merge_requests/46](https://gitlab.com/aossie/Agora/merge_requests/46)

* [MR48: Added Satisfaction Approval Voting and test][https://gitlab.com/aossie/Agora/merge_requests/48](https://gitlab.com/aossie/Agora/merge_requests/48)

* [MR49: Added proportional approval voting and test][https://gitlab.com/aossie/Agora/merge_requests/49](https://gitlab.com/aossie/Agora/merge_requests/49)

* [MR50: Added Sequential Proportional Approval Voting and test][https://gitlab.com/aossie/Agora/merge_requests/50](https://gitlab.com/aossie/Agora/merge_requests/50)

* [MR51: Added Oklahoma Method and test][https://gitlab.com/aossie/Agora/merge_requests/51](https://gitlab.com/aossie/Agora/merge_requests/51)

* [MR58: Candidate parser to accommodate party and id][https://gitlab.com/aossie/Agora/merge_requests/58](https://gitlab.com/aossie/Agora/merge_requests/58)

## Open

* [MR25: Added cumulative voting method][https://gitlab.com/aossie/Agora/merge_requests/25](https://gitlab.com/aossie/Agora/merge_requests/25)

* [MR42: Added Scottish STV][https://gitlab.com/aossie/Agora/merge_requests/42](https://gitlab.com/aossie/Agora/merge_requests/42)

* [MR52: Added Meek STV][https://gitlab.com/aossie/Agora/merge_requests/52](https://gitlab.com/aossie/Agora/merge_requests/52)

* [MR53: Added Warren STV][https://gitlab.com/aossie/Agora/merge_requests/53](https://gitlab.com/aossie/Agora/merge_requests/53)

* [MR60: Closed List Proportional Representation][https://gitlab.com/aossie/Agora/merge_requests/60](https://gitlab.com/aossie/Agora/merge_requests/60)

* [MR78: Added maximin method and test][https://gitlab.com/aossie/Agora/merge_requests/78](https://gitlab.com/aossie/Agora/merge_requests/78)

# Changes in Ballot and Parser methods

Since some algorithms required the need to have rank/score of candidates, I had decided to take the idea suggested in this [link](https://gitlab.com/aossie/Agora/issues/8) and extend it to the following ballot format : (Candidate;Rank;Score). Here, rank and score are made optional and specific parsers to parse the respective ballots have been made. Initially the ballot type was being sent as a different command line parameter, which later was modified into a more elegant solution, which is storing the election files with a ballot type specific extension( .e - election without rank and score, .es - election with score and without rank, .er - election with rank and without score, .esr - election with rank and score) which later gets parsed and then passed to the appropriate parser.

# Addition of various voting algorithms

I have added various voting algorithms that I found out on Wikipedia and Opavote. They involve Contingent method, Preferential Ballot voting, Instant Exhaustive Ballot, Nanson, Oklahoma method, Preferential, Satisfaction and Sequential Proportional Approval Voting etc. Also a few state-of-the-art STV algorithms like Meek and Warren algorithms have also been added. They were pretty straight-forward implementations.
 
# Changes in Candidate parser

The Candidate class already involved party and ID, but the same wasn’t done with the Candidate parser as there weren’t any party-list based algorithms implemented in Agora. I had planned on adding few party-list based algorithms starting with the Closed List Proportional Representation based algorithm for which I had to accommodate party and ID in the Candidate parser. This was again straight-forward.
 
# Addition of algorithms to calculate quotas

Party-list proportional representation based algorithms involve calculation of quotas using algorithms different from what is already present in Agora library. These are d’Hondt method, Webster method, LR-Droop method etc. Some of them have been implemented while some of them are yet to be done.

# Things left to tackle

Party-lists have not yet been properly tackled. Addition of scrutiny tables is done but might need modification in future for proper display (mainly for Meek and Warren STV). Summary tables for the rest of the algorithms are not created yet which will need some work and discussion with mentors and project admins.

I would like to thank my mentors at AOSSIE for being highly supportive and co-operative with me during the coding period because of which I enjoyed and learnt a lot. 
