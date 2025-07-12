#!/bin/bash
# -- SGE options :
#$ -S /bin/bash
#$ -cwd
#$ -t 1
#$ -V
#$ -q all.q
#$ -N SP_200parts
# --


#Running an SGE Array job so that we can run the scan in 200 parts in parallel
#qsub only works with the -q option

#Note: np defines into how many subsets pheno data is devided. 
#pIdx defines which of these parts is run.
#Thus, -t should correspond with pIdx. E.g. if pIdx=3, leave out -t or just put pIdx="$SGE_TASK_ID" and -t 3. 
#If you want to run all parts put pIdx="$SGE_TASK_ID" and -t 1-5 where the upper limit is equal to np.


#15.2.2023
#Jadwiga Buchwald

#1) Saving the phenotypes PHESANT derives.
#2) Using SGE to actually get 200 parallel jobs. 

#I manually created the directory "SavedPhenos" under this directory

#cd /[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/ALL
#qsub ./Scripts/1_SavePhenos_p1.sh



#--------------------------------------------
#Running PHESANT
#--------------------------------------------


#module add languages/R-3.3.1-ATLAS

#Print date
date

RDir="/[PATH]/"
RFuncDir="/[PATH]/jadwiga/RFunctions/"
PHESANTDir="/[PATH]/jadwiga/PHESANT/PHESANT-1.1/" 
DataDir="/[PATH]/jadwiga/Phenotypes/DataWrangling/Extracting_ALL/" 
ResDir="/[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/ALL/SavedPhenos/"

#part to be run (array number) and total number of parts 
#"$SGE_TASK_ID" is what I've set in the preamble for -t

pIdx="$SGE_TASK_ID"
np=200

echo "hello" $np $pIdx $SGE_TASK_ID > echo_${SGE_TASK_ID}_sge.txt



${RDir}R-3.3.1/bin/Rscript ${RFuncDir}phenomeScan_jb2Stage.R \
--phenofile="${DataDir}ukb_all_1_2_3_sorted_n343662_phesant_header.csv" \
--variablelistfile="${PHESANTDir}variable-info/outcome-info.tsv" \
--datacodingfile="${PHESANTDir}variable-info/data-coding-ordinal-info.txt" \
--resDir="${ResDir}" \
--userId="eid" \
--partIdx=$pIdx \
--numParts=$np \
--save

#Print date
date
