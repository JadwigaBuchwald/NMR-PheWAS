#Running the scripts one after the other.

#First manually created the folders:
	# Scripts (with this and the scripts to be ran)
	# ResultsFromSaved
	

#Running all of the below in the terminal:
	#cd /[PATH]/EVER


#note
#################################
#In the terminal:
	#cd /[PATH]/PHESANT/PHEWAS_2Stage/EVER
	#note: only got the hold option to work when refferring to the job id. Coudn't figure out how to refer to the jobname (For qsub! For grun.py refferring to the jobname worked, see the last part: combining results).
	#qsub -hold_jid 7639111 ./Scripts/2E_ScanFromSaved_p2to100.sh
	#This did not put it on hold:
	#qsub -hold_jid E_Scan_200parts.1 ./Scripts/2E_ScanFromSaved_p2to100.sh
	#Nor did this:
	#qsub -hold_jid ./Scripts/2E_ScanFromSaved_p1.sh ./Scripts/2E_ScanFromSaved_p2to100.sh
#################################



#Running part 1
	#Note:  Since i'm using sge to run an array job, the outputs and errors will be named in the form:
	#	#Scan_200parts.o[job number?].[part] & Scan_200parts.e[job number?].[part] as I had "-N Scan_200parts" in my below script
	#	#So for the below [part] will be 1

	#qsub ./Scripts/2E_ScanFromSaved_p1.sh

#Running parts 2-100 once the above has exited successfully

	#qsub -hold_jid 7639111 ./Scripts/2E_ScanFromSaved_p2to100.sh

#Running parts 101-200 once 2-100 job has exited successfully 

	#qsub -hold_jid 7639112 ./Scripts/2E_ScanFromSaved_p101to200.sh

#Running some parts that had not run correctly
#Rerunning EVER part 115 using the derived phenotypes as the error file said it was killed before it was fully complete

	#qsub ./Scripts/2E_ScanFromSaved_p115.sh

#Rerunning EVER part 119 using the derived phenotypes as the error file said it was killed before it was fully complete

	#qsub ./Scripts/2E_ScanFromSaved_p119.sh

#Rerunning EVER part 126 using the derived phenotypes as the error file said it was killed before it was fully complete
#/var/spool/gridengine/compute-2-87/job_scripts/7639595: line 42: 14420 Killed
#/var/spool/gridengine/compute-2-87/job_scripts/7639595: line 42: 14420 Killed 
#Now rerunning so that I changed the -q from all.q (default) to hugemem.q

	#qsub ./Scripts/2E_ScanFromSaved_p126.sh

#Combining results once 101-200 job has exited successfully. Note 101-200 job was named E_Scan_200parts in the "preamble"
#grun.py -n E_CombineResults --hold-jid E_Scan_200parts -c "./Scripts/3E_CombineResults.sh"

	
	
	#grun.py --hold_jid ./Scripts/1_Scan_p101to200.sh -n CombineResults ./Scripts/2_CombineResults.sh
