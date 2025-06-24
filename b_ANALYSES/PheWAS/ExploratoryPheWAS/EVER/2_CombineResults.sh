#Scan was run in parallel (200 jobs) on standardized exposure, confounders were age, sex and 10 PCs

#-------------------------------
#In the terminal
	#cd /[path]/EVER
	#grun -n CombineResults ./Scripts/2_CombineResults.sh
#-------------------------------


RDir="/[path]/"
RFuncDir="/[path]/RFunctions/"
PHESANTDir="/[path]/PHESANT-1.1/" 
ResDir="/[path]/EVER/Results/"


${RDir}R-3.3.1/bin/Rscript ${RFuncDir}mainCombineResults_jb.R \
--resDir="${ResDir}" \
--variablelistfile="${PHESANTDir}variable-info/outcome-info.tsv" \
--numParts=200