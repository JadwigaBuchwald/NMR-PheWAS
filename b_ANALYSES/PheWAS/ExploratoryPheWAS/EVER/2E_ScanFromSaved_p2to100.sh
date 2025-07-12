#!/bin/bash
# -- SGE options :
#$ -S /bin/bash
#$ -cwd
#$ -t 2-100
#$ -V
#$ -q all.q
#$ -N E_Scan_200parts
# --

#Running Ever parts 2-100 using the derived phenotypes

#In the terminal:
	#cd /[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/EVER
	#note: only got the hold option to work when refferring to the job id. Coudn't figure out how to refer to the jobname.
	#qsub -hold_jid 7639111 ./Scripts/2E_ScanFromSaved_p2to100.sh
	#This did not put it on hold:
	#qsub -hold_jid E_Scan_200parts.1 ./Scripts/2E_ScanFromSaved_p2to100.sh
	#Nor did this:
	#qsub -hold_jid ./Scripts/2E_ScanFromSaved_p1.sh ./Scripts/2E_ScanFromSaved_p2to100.sh

#Print date
date

RDir="/[PATH]/"
RFuncDir="/[PATH]/jadwiga/RFunctions/"
PHESANTDir="/[PATH]/jadwiga/PHESANT/PHESANT-1.1/" 
phenodir="/[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/ALL/SavedPhenos/"
TraitDir="/[PATH]/jadwiga/PHESANT/TRAIT/" 
ResDir="/[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/EVER/ResultsFromSaved/"
ConfDir="/[PATH]/jadwiga/Phenotypes/DataWrangling/Extracting_ALL/"


# part to be run (array number) and total number of parts
pIdx="$SGE_TASK_ID"
np=200


${RDir}R-3.3.1/bin/Rscript ${RFuncDir}PHESANT_from_derived/testFromSaved_jb.r \
--phenoDir="${phenodir}" \
--traitofinterestfile="${TraitDir}Ever_zGRS10SNPs_110348.csv" \
--traitofinterest="zGRS" \
--confounderfile="${ConfDir}ukb_n343662_phesant_confounders.csv" \
--resDir="${ResDir}" \
--userId="eid" \
--partIdx=$pIdx \
--numParts=$np 

#Print date
date
