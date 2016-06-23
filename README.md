This software implements some STV algorithms in a recursive way.
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
Examples of TERMINAL COMMANDS to run the code:
-----------------------------------------------------------------

sbt -J-Xmx4G -J-Xms4G 

run -d /home/users/u1017108/Code/PreferenceData/ACT/2012/ -f Preferences_ACT_Brindabella_2012.txt -a EVACS -n 5
run -d /home/users/u1017108/Code/PreferenceData/ACT/2012/ -f Preferences_ACT_Ginninderra_2012.txt -a EVACS -n 5
run -d /home/users/u1017108/Code/PreferenceData/ACT/2012/ -f Preferences_ACT_Molonglo_2012.txt -a EVACS -n 7

run -d /home/users/u1017108/Code/PreferenceData/ACT/2008/ -f Preferences_ACT_Brindabella_2008.txt -a EVACS -n 5
run -d /home/users/u1017108/Code/PreferenceData/ACT/2008/ -f Preferences_ACT_Ginninderra_2008.txt -a EVACS -n 5
run -d /home/users/u1017108/Code/PreferenceData/ACT/2008/ -f Preferences_ACT_Molonglo_2008.txt -a EVACS -n 7

run -d /home/users/u1017108/Code/PreferenceData/ACT/2012/ -f Preferences_ACT_Brindabella_2012.txt -a Simple -n 5
run -d /home/users/u1017108/Code/PreferenceData/ACT/2012/ -f Preferences_ACT_Ginninderra_2012.txt -a Simple -n 5
run -d /home/users/u1017108/Code/PreferenceData/ACT/2012/ -f Preferences_ACT_Molonglo_2012.txt -a Simple -n 7


run -d /home/users/u1017108/ownCloud/Code/files/wikipedia/ -f wikipedia.txt -a EVACS -n 3
run -d /home/users/u1017108/ownCloud/Code/files/wikipedia/ -f wikipedia.txt -a Simple -n 3



-----------------------------------------------------------------
TODO:
-----------------------------------------------------------------

EVACS:
1) Surplus distribution tie resolution for EVACS (history of scrutiny is required)
2) Exclusion tie resolution for EVACS (history of scrutiny is required)
3) Imitation of EVACS's wrong totals that appear in their scrutiny
4) Distribution of votes of the last excluded candidate when the number of continuing candidates becomes equal to the number of remaining vacancies

GENERAL:
 
1) Output the scrutiny as a table
2) In the following line of tryToDistributeSurplusVotes
  val newElection = removeWinnerFromElection(election, winner) 
the candidate should be removed from all preferences in ballots
3) Take care in TransferValue traits that also winners are taken into account when we check continuing ballots....

