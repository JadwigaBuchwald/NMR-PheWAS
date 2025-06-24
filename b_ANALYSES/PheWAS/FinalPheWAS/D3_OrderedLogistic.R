#31.3.2023
#Jadwiga Buchwald

# Results for ordered logistic: Getting them into the format required for plotting the forest plot and for our results table.

# We had 12 variables, now we should have 8.
# -4 were moved to linear (coffee went into binary as well): 2887 Number of cigarettes previously smoked daily, 3456 Number of cigarettes currently smoked daily, 30150 Eosinophill count, 1498 Coffee intake  
# -two need their sign changed:3506 smoking compare to 10 yrs previous & 1249 Past tobacco smoking 
# -Note: I also checked the coding for 1239 Current tobacco smoking coding and PHESANT has already corrected it!


# STEPS IN THIS DOCUMENT
# a) Getting our variables and the wanted columns
# b) Correcting the sign of the beta for the two variables needing correction -->Adding variable newvarName where we add c to these variables in front of varName
#    Note: We also add a c in front of 1239 Current tobacco smoking as we checked the coding and noticed that PHESANT had already corrected it.
# c) Checking the FDR significance
# d) Getting the ShortName and creating the variable Phenotype
# e) Creating the Results version table and Getting the EvsN p-values and creating the EverNeverSig variable
# f) creating our data for the forest plot and for our results table


#DOing this interactively

#cd /[path]
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R

#-----------------------------------
#Rscript
#-----------------------------------

#---------------------------------
#Not for Github:

WorkDir="/[path]/"

#----------------------------------


sessionInfo()
getwd()
#libraries
	library(plyr)
	library(dplyr)
	library(tidyr)

#----------------------------------------------------------------------------------------


#----------a) Getting our variables and the wanted columns------------------------------

   ---RESULTS---
	#Reading in the results
		Init = read.csv(paste(WorkDir,"FDR_initial/IntermediateData/PHESANT_InitialResults_forForestquote.csv",sep=""), as.is=T, header=T)
		str(Init)
		dim(Init) 61 x 26

	#We get the wanted variables - ordered-logistic set without the four that were moved to linear.
		Init<-subset(Init,Init$resType=="ORDERED-LOGISTIC")
		dim(Init) 12 x 26
		OResults<- subset(Init,!(varName %in% c(2887,3456,30150,1498)))
		dim(OResults) #8 26

   ---FOREST---
	#Reading in the result taht were in the format for forest plotting
		InitF = read.csv(paste(WorkDir,"FDR_initial/IntermediateData/PHESANT_InitialResults_forForestquote_stacked.csv",sep=""), as.is=T, header=T)
		str(InitF)
		dim(InitF) 170 x 12

	#We get the wanted variables - ordered-logistic set without the four that were moved to linear.
		InitF<-subset(InitF,InitF$resType=="ORDERED-LOGISTIC")
		dim(InitF) 30 x 12
		OForest<- subset(InitF,!(varName %in% c(2887,3456,30150,1498)))
		dim(OForest) #20 12

#TO KEEP THING SIMPLE I'LL WORK ON THE FOREST VERSION AND IN THE END RECREATE A RESULTS VERSION OUT OF THAT AND SAVE BOTH VERSIONS

#-----------# b) Correcting the sign of the beta for the two variables needing correction ----------------------------------------------------------------
		& Adding variable newvarName where we add c to these variables in front of varName
#    		Note: We also add a c in front of 1239 Current tobacco smoking as we checked the coding and noticed that PHESANT had already corrected it.
-----------------------------------------------------------------------------------------------------------------------------------------------------------

	# I noticed I was missing the se from the Forest version so let's get the se for the beta at this stage
	
 		OForest$se<-(OForest$beta-OForest$lower)/1.96


	# Correcting the sign of the beta for the two needing correction

		# Checking the two needing correction
			subset(OForest,OForest$varName ==3506)
			subset(OForest,OForest$varName ==1249)


		#We create the corrected version
			OLFc<-OForest

			#Finding the rows we need to correct
				which(OLFc$varName %in% c(3506,1249)) # 5  6 13 14 21 22

			#Multiplying by -1
				OLFc[(OLFc$varName %in% c(3506,1249)),c("beta","lower","upper")]<-OLFc[(OLFc$varName %in% c(3506,1249)),c("beta","lower","upper")]*-1

			#Then have to switch lower and upper around:
				OLFc[(OLFc$varName %in% c(3506,1249)),c("lower","upper")]<-OLFc[(OLFc$varName %in% c(3506,1249)),c("upper","lower")]

			#We create newvarName and add a c infront of it for those with varName %in% c(3506,1249,1239)
				OLFc$newvarName<-ifelse(OLFc$varName %in% c(3506,1249,1239), paste("c",OLFc$varName,sep=""),OLFc$varName)

#-------------------# c) Checking the FDR significance------------------------------------------

	#We add the variables FDRsig & NotFDRsig 
		OLFc$FDRsig <- ifelse(OLFc$fdrp<0.05,1,0)
		OLFc$NotFDRsig <- ifelse(OLFc$fdrp>=0.05,1,0)


--------------------# d) Getting the ShortName and creating the variable Phenotype-------------------------

	ShortNames<-read.table(paste(WorkDir,"FDR_initial/IntermediateData/ShortNames61Variables.txt",sep=""),sep="\t",quote="\"",header=TRUE)

	F<-join(OLFc, ShortNames[,c("varName","ShortName")], by="varName", type="left")
	F$Phenotype <- paste(F$newvarName,F$ShortName,sep=" - ")

-------------------# e) Creating the Results version table and Getting the EvsN p-values and creating the EverNeverSig variable------------------------

	#Creating the results version table
		#We get the ALL subset
			ALL <- subset(F,F$data=="ALL")
			ALL<-ALL[order(ALL$Phenotype),]
			ALLwanted <- ALL[,c("Phenotype","resType","Cat3_Title","beta","lower","upper","se","pvalue","n","FDRsig")]
			names(ALLwanted) <- c("Phenotype","resType","Cat3_Title","beta_A","lower_A","upper_A","se_A","pvalue_A","n_A","FDRsig_A")
			ALL_end <- ALL[,c("Phenotype","varName","description","ShortName")]
				

		#Ever
			EVER <- subset(F,F$data=="EVER")
			EVERwanted <- EVER[,c("Phenotype","beta","lower","upper","se","pvalue","n","FDRsig")]
			names(EVERwanted) <- c("Phenotype","beta_E","lower_E","upper_E","se_E","pvalue_E","n_E","FDRsig_E")
			EVERwanted<-EVERwanted[order(EVERwanted$Phenotype),]

		#Never
			NEVER <- subset(F,F$data=="NEVER")
			NEVERwanted <- NEVER[,c("Phenotype","beta","lower","upper","se","pvalue","n","FDRsig")]
			names(NEVERwanted) <- c("Phenotype","beta_N","lower_N","upper_N","se_N","pvalue_N","n_N","FDRsig_N")
			NEVERwanted<-NEVERwanted[order(NEVERwanted$Phenotype),]
		
		#We join the results 
			Results<-join(ALLwanted, EVERwanted, by="Phenotype", type="left")
			Results<-join(Results, NEVERwanted, by="Phenotype", type="left")
			Results<-join(Results, ALL_end, by="Phenotype", type="left")
			dim(Results)

	#Creating FDRsigSum
		Results$FDRsigSum <- rowSums(Results[,c("FDRsig_A", "FDRsig_E","FDRsig_N")], na.rm=TRUE)
		table(Results$FDRsigSum)
			# 1 2
			# 5 3

	#EvervsNever
	#As these are all just the same results as we got with PHESANT, we already know none are going to get a stat. sig. difference when comparing the betas between Ever and Never
	#However, I'll just rerun so we get the pvalues for our result table

		threshold_EvsN <- 0.05*2/15212
		Results$x = (Results$beta_E-Results$beta_N)^2 / (Results$se_E^2+Results$se_N^2)
      		Results$pvalue_EvsN = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) #8 x 30

		Results$EverNeversig <- ifelse(Results$pvalue_EvsN < threshold_EvsN,1,0)
		dim(subset(Results, Results$EverNeversig==1)) #0 x31

-----------------------------# f) creating our data for the forest plot and for our results table---------------------------------------------------------

--------------RESULTS-----------

	#Ordering based on Cat3_Title and Pheno
		RESULTS <- Results[order(Results$Cat3_Title,Results$Phenotype),]

	#Get wanted columns in wanted order
		RESULTS <- RESULTS[,c("Phenotype", "resType", "Cat3_Title",
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", "FDRsig_A",
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", "FDRsig_E",
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", "FDRsig_N",
				"varName", "description", "ShortName", "FDRsigSum","pvalue_EvsN", "EverNeversig")]

#We save the results file as a supplementary table
#We create the following readme_Final_8_OrderedLogisticResults.txt to accompary it (can then copy paste this to another sheet inside the excel)
Phenotype: d = derived, n = normalised 
resType: regression model used
Cat3_Title: The UKBB category
beta: coefficient for the GRS from the regression model
lower and upper: 95 % Confidence interval
p_value: p-value for the beta listed
n: sample size as obtained from PHESANT 
FDRsig: Yes=1 No=0 - whether or not the p_value was below the 5 % FDR threshold.
A: Dataset All
E: Dataset Ever
N: Dataset Never
varName: Data Field in UKBB
description: description as in the UKBB or has been created for derived variables
ShortName: shorter version of description for plotting purposes
FDRsigSum: In how many of the three analyses(ALL/EVER/NEVER) was the result statistically significant at the 5 % FDR level
pvalue_EvsN: P-value for the comparison of the Ever vs Never results (when both were available)
EverNeversig: Yes=1 No=0 -whether or not the difference between the betas from the Ever and Never results was statistically significant at the 5% FDR level. 



#We save the combined datasets
	#Results table that has to be concatenated with the binary and linear ones. 
		write.table(RESULTS,"./IntermediateData/Final_8_OrderedLogisticResults.txt",sep="\t",quote=TRUE,row.names=FALSE) 
		write.table(RESULTS,"./IntermediateData/Final_8_OrderedLogisticResults.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)


--------------FOREST-----------
	

	#NOTE have to change in F ALL-->All and EVER-->Ever and NEVER-->Never
		Forest <- F
		Forest$data<-recode(Forest$data, "ALL" = "All", "EVER" = "Ever", "NEVER" = "Never")

	
	#Ordering based on Cat3_Title and Pheno
		Forest <- Forest[order(Forest$Cat3_Title,Forest$Phenotype),]

	#Get wanted columns in wanted order
		Forest <- Forest[, c("Phenotype", "beta" ,"se", "NotFDRsig", "data", "Cat3_Title", "resType","pvalue","varName")]

#We create the following readme_Final_8_OrderedLogisticForest.txt to accompary it (can then copy paste this to another sheet inside the excel)
Phenotype: d = derived, n = normalised 
beta: coefficient for the GRS from the regression model
se: standard error for beta
NotFDRsig: 1=Yes (i.e. not FDR significant) 0=No (ie. IS FDR significant) This will be used to fill in the circles in the plot command (fill if value below 0.05)
data: which dataset, All/Ever/Never
Cat3_Title: The UKBB category
---the below should not be needed for plotting-------
resType: regression model used
pvalue: pvalue for the beta
varName: Data Field in UKBB

#Table for creating the Forest plot
	write.table(Forest,"./IntermediateData/Final_8_OrderedLogisticForest.txt",sep="\t",quote=TRUE,row.names=FALSE) 
	write.table(Forest,"./IntermediateData/Final_8_OrderedLogisticForest.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)
