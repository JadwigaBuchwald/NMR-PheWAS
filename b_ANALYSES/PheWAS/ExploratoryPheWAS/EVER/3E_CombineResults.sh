#16.2.2023
#Jadwiga Buchwald
#Scan was run in parallel (200 jobs) on standardized exposure, confounders were age, sex and 10 PCs


#cd /[PATH]/PHESANT/PHEWAS_2Stage/EVER/
#grun.py -n E_CombineResults --hold-jid E_Scan_200parts -c "./Scripts/3E_CombineResults.sh"

RDir="/[PATH]/"
RFuncDir="/[PATH]/jadwiga/RFunctions/"
PHESANTDir="/[PATH]/jadwiga/PHESANT/PHESANT-1.1/" 
ResDir="/[PATH]/jadwiga/PHESANT/PHEWAS_2Stage/EVER/ResultsFromSaved/"


${RDir}R-3.3.1/bin/Rscript ${RFuncDir}PHESANT_from_derived/mainCombineResults_jb.R \
--resDir="${ResDir}" \
--variablelistfile="${PHESANTDir}variable-info/outcome-info.tsv" \
--numParts=200