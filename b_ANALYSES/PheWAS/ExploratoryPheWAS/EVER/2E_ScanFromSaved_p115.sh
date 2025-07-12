#!/bin/bash
# -- SGE options :
#$ -S /bin/bash
#$ -cwd
#$ -t 115
#$ -V
#$ -q all.q
#$ -N E_Scan_200parts
# --

#Rerunning EVER part 115 using the derived phenotypes as the error file said it was killed before it was fully complete

#In the terminal:
	#cd /[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/EVER
	#qsub ./Scripts/2E_ScanFromSaved_p115.sh

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
