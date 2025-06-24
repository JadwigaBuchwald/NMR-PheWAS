#Running the scripts one after the other.

#First manually created the folders:
	# Scripts (with this and the scripts to be ran)
	# Results
	

#Running all of the below in the terminal:
	#cd /[PATH]/EVER


#Running part 1
	#Note:  Since i'm using sge to run an array job, the outputs and errors will be named in the form:
	#	#Scan_200parts.o[job number?].[part] & Scan_200parts.e[job number?].[part] as I had "-N Scan_200parts" in my below script
	#	#So for the below [part] will be 1

	#qsub ./Scripts/1_Scan_p1.sh

#Running parts 2-100 
#Running parts 101-200 once 2-100 job has exited successfully 
#Combining results once 101-200 job has exited successfully

	#qsub ./Scripts/1_Scan_p2to100.sh
	#qsub -hold_jid ./Scripts/1_Scan_p2to100.sh ./Scripts/1_Scan_p101to200.sh
	#grun.py --hold_jid ./Scripts/1_Scan_p101to200.sh -n CombineResults ./Scripts/2_CombineResults.sh
