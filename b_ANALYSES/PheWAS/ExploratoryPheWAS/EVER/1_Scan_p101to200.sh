#!/bin/bash
# -- SGE options :
#$ -S /bin/bash
#$ -cwd
#$ -t 101-200
#$ -V
#$ -q all.q
#$ -N Scan_200parts
# --

#Running an SGE Array job so that we can run the scan in 200 parts in parallel
#qsub only works with the -q option

#Note: np defines into how many subsets pheno data is devided. E.g. if np=5 phenotypes devided into 5 equal sized parts. 
#pIdx defines which of these parts is run.
#Thus, -t should correspond with pIdx. E.g. if pIdx=3, leave out -t or just put pIdx="$SGE_TASK_ID" and -t 3. 
#If you want to run all parts put pIdx="$SGE_TASK_ID" and -t 1-5 where the upper limit is equal to np.

#Running parts 101-200.
#1) Running scan (not saving the derived phenotypes)
#2) Using SGE to actually get 200 parallel jobs. 

#I manually created the directory "Results" under this directory

#In the terminal
	#cd /[path]/EVER
	#qsub ./Scripts/1_Scan_p101to200.sh



#--------------------------------------------
#Running PHESANT
#--------------------------------------------

#we use standardised exposure and adjust for age, sex and 10PCs (confounders found in confounder file)

#Print date
date

RDir="/[PATH]/"
RFuncDir="/[PATH]/RFunctions/"
PHESANTDir="/[PATH]/PHESANT-1.1/" 
DataDir="/[PATH]/DATA/"
TraitDir="/[PATH]/TRAIT/" 
ResDir="/[PATH]/EVER/Results/"
ConfDir="/[PATH]/DATA/"


# part to be run (array number) and total number of parts
pIdx="$SGE_TASK_ID"
np=200

echo "hello" $np $pIdx $SGE_TASK_ID > echo_sge.txt



${RDir}R-3.3.1/bin/Rscript ${RFuncDir}phenomeScan_jb.R \
--phenofile="${DataDir}ukb_all_1_2_3_sorted_n343662_phesant_header.csv" \
--variablelistfile="${PHESANTDir}variable-info/outcome-info.tsv" \
--traitofinterestfile="${TraitDir}Ever_zGRS10SNPs_110348.csv" \
--datacodingfile="${PHESANTDir}variable-info/data-coding-ordinal-info.txt" \
--traitofinterest="zGRS" \
--confounderfile="${ConfDir}ukb_n343662_phesant_confounders.csv" \
--resDir="${ResDir}" \
--userId="eid" \
--partIdx=$pIdx \
--numParts=$np 

#Print date
date

