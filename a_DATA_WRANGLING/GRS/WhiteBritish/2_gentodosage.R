#17.1.2022
#Jadwiga Buchwald

#Converting gen files to dosage format (using the function in GenotypeFileConversion.R)

#################################
#----------
#interactively in bash
#----------

#change directory: 
#cd /[path]

#Declaring directory:
#RDir="/[path]/"

#Running the R script
#grun -n gentodosage ${RDir}R-4.1.1/bin/R CMD BATCH ./gentodosage.R

#################################




#-----------------------------------------------------------------------
#Rscript for running the function
#-----------------------------------------------------------------------

sessionInfo()
getwd()


#FUNCTIONS:
source("../../../RFunctions/GenotypeFileConversion.R")


#GRS chr19 SNPs
GenToDosage(Genin="topconfig9SNPs.gen", Samplein="../../../Genotypes/UKB_chr19/UK_zrsids_WB20210809.sample", Dosageout="GRSchr19SNPs_WB20210809_dosage")
