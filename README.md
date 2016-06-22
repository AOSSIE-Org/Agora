TODO:

1) Surplus distribution tie resolution for EVACS (history of scrutiny is required)
2) Exclusion tie resolution for EVACS (history of scrutiny is required)
3) Imitation of EVACS's wrong totals that appear in their scrutiny
4) Distribution of votes of the last excluded candidate when the number of continuing candidates becomes equal to the number of remaining vacancies 
5) Output the scrutiny as a table

-----------------------------------------------------------------
Terminal commands to run:


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
