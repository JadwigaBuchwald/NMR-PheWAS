#1.2.2021
#Checking the wanted R version and required R packages work

#-----------------------
#INTRODUCTION
#-----------------------


#Instructions for using PHESANT:https://github.com/MRCIEU/PHESANT/tree/v1.1 (or PHESANT-1.1/README.md)
#For doing their example: PHESANT-1.1/testWAS/README.md for an example with test data.

#-----------------------
#From the README.md:
#1. Running a phenome scan in UK Biobank
#2. Post-processing of results
#3. PHESANT-viz: Visualising the results
#
#
### General requirements
#
#R for parts 1 and 2 above. Tested with R-3.3.1-ATLAS. Phenome scan requires the R packages: optparse (V1.3.2), MASS (V7.3-45), lmtest (V0.9-34), nnet (V7.3-12), forestplot (V1.7) and data.table (V1.10.4).
#
#Java for part 3 above. Tested with jdk-1.8.0-66.
#-----------------------

#-----------------------
#SUMMARY:
#versions and R packages needed and what we have
#	#RVERSION: 
#		#R-3.3.1-ATLAS
#			#Check
#
#	#PACKAGES: 
#
#	#The below three already existed in the R library
#		#MASS (V7.3-45)
#			#Check
#		#nnet (V7.3-12)
#			#Check
#		#data.table (V1.10.4)
#			#Our version:(1.10.4-3)
#
#	#These three I installed in 2019 
#		#optparse (V1.3.2)
#			#Our version (1.6.2)
#		#lmtest (V0.9-34)
#			#Our version (0.9.37)
#		#forestplot (V1.7)
#			#Our version (1.7.2)
#
#---------------------------------------------------------------------------------



#-----------------------
#ACTUAL SCRIPT
#-----------------------


#---------------------------------------------------------------------------------
#In bash
#cd /[path]
#RDir="/[path]/"

#grun -n B_CheckingRequirements ${RDir}R-3.3.1/bin/R CMD BATCH B_CheckingRequirements.R

#---------------------------------------------------------------------------------

#------------------
#Starting by checking the wanted R version works.
#------------------
print("hello")
sessionInfo()

#------------------
#Then checking all the packages that should already exist work
#------------------
.libPaths("/[path]/R-3.3.1/library")

library(MASS)
packageVersion("MASS")

library(nnet)
packageVersion("nnet")

library(data.table)
packageVersion("data.table")


#------------------
#Then loading the rest of the packages needed and checking the version 
#------------------
#These were installed in 2019 so not going to reinstall them. Will just check that they work.
.libPaths("/[path]")
#chooseCRANmirror(ind=67)

#install.packages("optparse")
library("optparse")
packageVersion("optparse")

#install.packages("lmtest")
library("lmtest")
packageVersion("lmtest")

#install.packages("forestplot")
library("forestplot")
packageVersion("forestplot")