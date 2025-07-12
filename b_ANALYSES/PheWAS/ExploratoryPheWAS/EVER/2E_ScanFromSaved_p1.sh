#!/bin/bash
# -- SGE options :
#$ -S /bin/bash
#$ -cwd
#$ -t 1
#$ -V
#$ -q all.q
#$ -N E_Scan_200parts
# --

#Running EVER part 1 using the derived phenotypes

#In the terminal:
	#cd /[PATH]/PHESANT/PHEWAS_2Stage/EVER
	#qsub ./Scripts/2E_ScanFromSaved_p1.sh

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
