#17.4.2023
#Jadwiga Buchwald

#Since there was a stat sig. difference between the effect sizes of the ever vs never smokers for four FEV variables
#--> Following up on these results by checking if there is a differenece between current vs former smokers.

#---------------------------------------------------------------------------------------

#Doing this interactively

#cd /[path]/PHESANT/PHEWAS_2Stage/FDR_final
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R


#-----------------------------------
#Rscript
#-----------------------------------

#---------------------------------
#Not for Github:

DataDir="/[path]/Phenotypes/DataWrangling/Extracting_ALL/"
WorkDir="/[path]/PHESANT/PHEWAS_2Stage"
TraitDir="/[path]/PHESANT/TRAIT/"

#----------------------------------

sessionInfo()
getwd()
#libraries
	library(plyr)
	library(dplyr)
	library(tidyr)

#----------------------------------------------------------------------------------------

#######################################################################################################################################
# READING IN THE DATA 
#######################################################################################################################################

#a) 	Getting out trait data for the current smokers and the former smokers seperately

	#i) 	Reading in the the cessation variable i.e. 1= Has quit, i.e. former smoker 0= Had not quit, i.e. current smoker

		DP<-read.table(paste(WorkDir,"/FDR_final/IntermediateData/DerivedBinaryPhenos.txt",sep=""), sep="\t",header=TRUE)
		dim(DP)	 # 343662     18
		D <- DP[,c("eid","x20116_0_0","d20116_Cessation")]


		#> table(D$d20116_Cessation)
		#
		#     0      1
		# 34610 120786
		#> table(D$x20116_0_0)
		#
		#    -3      0      1      2
		#  1188 187078 120786  34610


 	#ii) Reading in the GRS data for Ever

		TE = read.csv(paste(TraitDir,"Ever_zGRS10SNPs_110348.csv",sep=""), as.is=TRUE, header=TRUE)

	#iii) 	Subsetting the trait data
		
		T <- join(TE,D, by = "eid", type ="left")

			table(T$d20116_Cessation)
			#    0     1
			#29141 81179
		
			table(T$x20116_0_0)
			#   -3     1     2
			#   28 81179 29141

		TC <- subset(T, T$d20116_Cessation == 0)
		dim(TC) # 29141     4

		TF <- subset(T, T$d20116_Cessation == 1)
		dim(TF) # 81179     4


#d)	Reading in the covariates
		
		C = read.csv(paste(DataDir,"/ukb_n343662_phesant_confounders.csv",sep=""), as.is=TRUE, header=TRUE)
		names(C)
		str(C)
		dim(C) #343662     13


#c)	Reading in the FEV variables

#		PhenoTrait <- read.table(paste(WorkDir,"/FDR_final/IntermediateData/Linear_rtnPhenos_ForAnalysis.txt",sep=""), sep="\t",header=TRUE)
		#--> unfortunately I had already deleted this file so recreating it but I'll just do it for the wanted 4 FEV variables:
			P = read.csv(paste(WorkDir,"/FDR_final/IntermediateData/ukb_sorted_n343662_61variables.csv",sep=""))

			dim(P) #343662    415
			names(P[1:10,1:10])
			
			#Only keeping eid and the four FEV variables
			N<-P[,c("eid","x20154_0_0","x20256_0_0","x20150_0_0","x3063_0_0")]
			dim(N) #343662      5

			head(N)
			str(N)
			summary(N[,2:5])
			rm(P)

			#-------------------------------
			#adjusting for confounders and then normalizing.
			#-------------------------------

			#NOTE this differs from PHESANT: We adjust for covariates FIRST and only THEN normalize the data

			#We use the rank based inverse normal transformation instead of just creating z-scores ((x-mean)/sd, which could be done e.g. using the scale function in R)
			#PHESANT also uses the rank based inverse normal transformation, and we use the function as they do:
			#See the function defined at the bottom of the script PHESANT-1.1/WAS/testContinuous:
			#https://github.com/MRCIEU/PHESANT/blob/master/WAS/testContinuous.r

			#I'll later add a "n" in front of all of these variables to show that they were normalized in a different way (i.e. we FIRST adjusted for confounders and THEN normalized)

			#-----------------------------------------------------------------------------
			#defining the function to be used once we have obtained the residuals:
				#find INT-transformed residuals
				irnt <- function(pheno) {
					set.seed(1234)
					numPhenos = length(which(!is.na(pheno)))
					quantilePheno = (rank(pheno, na.last="keep", ties.method="random")-0.5)/numPhenos
					phenoIRNT = qnorm(quantilePheno)	
					return(phenoIRNT);
				}

				#example of use: rtn_resCPD = irnt(resCPD)
			#-----------------------------------------------------------------------------

			#Reading in trait data for the All dataset (as we want to normalize using the All dataset)
			TA = read.csv(paste(TraitDir,"zGRS10SNPs_topconfig.csv",sep=""), as.is=TRUE, header=TRUE)
			dim(TA) # 343662      2

			
			#1) We adjust for the covariates and then normalize. I.e. we perform inverse normal transformation of the trait residuals

			Data<-TA
			for (i in 2:5) {
				#Creating a merged dataset (outcome+covariates)
						pname<-names(N)[i]
						p_i<-N[,c(1,i)]
						D <- join(p_i,C, by = "eid", type ="left")
						D<- D[complete.cases(D),]
						print(names(D))
						print(dim(D))
		
				#Preparing the data for our analysis
						pheno = D[,pname]
						confNames = colnames(C)
			        		confNames = confNames[-which(confNames=="eid")]
						confs = D[,confNames]

				#Adjusting for covariates
					fit <- lm(pheno ~ ., data=confs)
	
				#Taking the adjusted pheno
					respheno <- residuals(fit)

				#Normalizing
					rtn_respheno = irnt(respheno)

				#Saving the new phenotype
					S<-as.data.frame(cbind(D[,"eid"],rtn_respheno))
					names(S)<-c("eid",paste("n",pname,sep=""))
					Data<-join(Data,S,by = "eid", type="left")
					print(names(Data))
			}


		#I'll save these derived variables just in case (We'll remove this once we have our results and plots)
		write.table(Data,"./IntermediateData/Linear_rtnPhenos_ForAnalysis.txt",sep="\t",quote=F,row.names=F) 


#######################################################################################################################################
# Doing the analysis
#######################################################################################################################################

#-----------------------function: testLinear----------------------------------------------

#Note: Remember that a function can only return one object! 
#That is why I'm returning the data for the forest plot and for the results table in the same dataframe and then dividing them into two after using the function.


#Trait data will define the dataset CURRENT/FORMER
testLinear<-function(dataset, Beta, Pvalue, Se, N, Lower, Upper, Phenodata, Traitdata){
	
	OUT<-data.frame()
		for (i in names(Phenodata[,-1])) {

		workName<-i
		resType<-"LINEAR"
		
		data=dataset

		#Creating a merged dataset (exposure+outcome+covariates)
			p_i<-Phenodata[,c("eid", i)]
			D <- join(Traitdata,p_i, by = "eid", type ="left")
			names(D)
			dim(D)

		#Preparing the data for our analysis
			pheno = D[,i]
			exp = D[,"zGRS"]
			
		#Getting the n
			n = length(which(!is.na(pheno)))

		#Running the logistic regression
			mylm <- lm(pheno ~ exp)

		#Getting the results
			sumx = summary(mylm)
		
			beta = sumx$coefficients["exp","Estimate"]
                	pvalue = sumx$coefficients["exp","Pr(>|t|)"]
			se = sumx$coefficients["exp","Std. Error"]
                
                	cis = confint(mylm, level=0.95)
                	lower = cis['exp',"2.5 %"]
                	upper = cis['exp',"97.5 %"]

		OUT[which(names(Phenodata[,-1])==i),1:12]<-c(workName, resType, beta, se, pvalue, data, beta, lower, upper, se, pvalue, n)
		print(OUT)
		}

	names(OUT)<-c("workName", "resType", "beta", "se", "pvalue", "data",Beta, Lower, Upper, Se, Pvalue, N)
	return(OUT) 
				
}


#-----------------------------------------------------------------------------------------
#Reading in eid and the phenos we want to analyse
	PhenoTrait<-Data
	PhenoTrait<-read.table("./IntermediateData/Linear_rtnPhenos_ForAnalysis.txt", sep="\t",header=TRUE)


#For Current
	O<-testLinear(dataset="Current", Beta="beta_C", Pvalue="pvalue_C", Se="se_C", N="n_C", Lower="lower_C", Upper="upper_C", Phenodata=PhenoTrait[,-2], Traitdata=TC)
	FD_c<-O[,c(1,2,3:6)]
	RD_c<-O[,c(1,2,7:12)]
	FD_c
	RD_c

	write.table(FD_c,"./IntermediateData/DerivedLinearForest_C.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_c,"./IntermediateData/DerivedLinearResults_C.txt",sep="\t",quote=F,row.names=F) 




#For Former
#Note: have to exclude initiation(9) 
	O<-testLinear(dataset="Former", Beta="beta_F", Pvalue="pvalue_F", Se="se_F", N="n_F", Lower="lower_F", Upper="upper_F", Phenodata=PhenoTrait[,-2], Traitdata=TF)
	FD_f<-O[,c(1,2,3:6)]
	RD_f<-O[,c(1,2,7:12)]
	FD_f
	RD_f

	write.table(FD_f,"./IntermediateData/DerivedLinearForest_F.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_f,"./IntermediateData/DerivedLinearResults_F.txt",sep="\t",quote=F,row.names=F) 

#######################################################################################################################################
#e) Merging the Current/Former results into combined files 
#######################################################################################################################################
#Then we a) concatenate the three FD (Forestplot data) datasets by workName 
#And b) merge the three RD (Results table data) datasets 


	FD_f<-read.table("./IntermediateData/DerivedLinearForest_F.txt", sep="\t",header=TRUE)
	FD_c<-read.table("./IntermediateData/DerivedLinearForest_C.txt", sep="\t",header=TRUE)

	RD_c<-read.table("./IntermediateData/DerivedLinearResults_C.txt", sep="\t",header=TRUE)
	RD_f<-read.table("./IntermediateData/DerivedLinearResults_F.txt", sep="\t",header=TRUE)

	dim(FD_c); dim(FD_f);  #4 x 6 
	dim(RD_c); dim(RD_f);  #4 x 8 


#Forest:
	Forest<-rbind(FD_c,FD_f)
	F<-Forest[order(Forest$workName),]

	F$varNamelong<-substring(F$workName,3) #starting from the third character we copy workName to varNamelong, i.e we drop the nx and nd.
	F<-separate(F, varNamelong, into = c("varName", "ending"), sep = "_", extra="merge") #So I can get Cat3 and ShortName from another file
	F$newvarName<-paste("n",F$varName,sep="") 
	
#Results:
	names(RD_c)
	#We join the results (we delete resType from the former not to have that multiple times)
	Results<-join(RD_c, RD_f[,-2], by="workName", type="left")

	Results$varNamelong<-substring(Results$workName,3) #starting from the third character we copy workName to varNamelong, i.e we drop the nx and nd.
	Results<-separate(Results, varNamelong, into = c("varName", "ending"), sep = "_", extra="merge") #So I can get Cat3 and ShortName from another file
	Results$newvarName<-paste("n",Results$varName,sep="") 

#######################################################################################################################################
# Running the ever vs never analysis and comparing to ever vs never fdr significance
#######################################################################################################################################


		threshold_EvsN <- 0.05*2/15212 #6.573758e-06
		Results$x = (Results$beta_C-Results$beta_F)^2 / (Results$se_C^2+Results$se_F^2)
      		Results$pvalue_CvsF = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) 

		Results$CurrentFormersig <- ifelse(Results$pvalue_CvsF < threshold_EvsN,1,0) #All got zero

		
#######################################################################################################################################
g) Getting the Cat3_Title + Description+ ShortName & creating the Pheno variable --> creating our data for the forest plot and for our results table
#######################################################################################################################################

--------------------# Getting the ShortName and creating the variable Phenotype-------------------------

	ShortNames<-read.table(paste(WorkDir,"/FDR_initial/IntermediateData/ShortNames61Variables.txt",sep=""),sep="\t",quote="\"",header=TRUE)


#We get Cat3_Title and ShortName for all variables
		#We delete "resType"  from ShortNames before merging (for now)
		RESULTS<-join(Results, ShortNames[,-c(2)], by="varName", type="left")

#We create Pheno 
		RESULTS$Phenotype <- paste(RESULTS$newvarName, RESULTS$ShortName, sep=" - ")

	

#Only keeping wanted columns 

	RESULTS <- RESULTS[,c("Phenotype", "resType", "Cat3_Title", 
				"beta_C", "lower_C", "upper_C", "se_C", "pvalue_C", "n_C",
				"beta_F", "lower_F", "upper_F", "se_F", "pvalue_F", "n_F",
				"varName", "description", "ShortName", "pvalue_CvsF")]
		

#Ordering based on Cat3_Title and Pheno
	RESULTS <- RESULTS[order(RESULTS$Phenotype),]


#We save the results file as a supplementary table
#We create the following readme_CurrentFormer_4FEVvariables_LinearResults.txt to accompary it (can then copy paste this to another sheet inside the excel)
Phenotype: n = normalised 
resType: regression model used
Cat3_Title: The UKBB category
C=Current, F=Former
beta: coefficient for the GRS from the regression model
lower and upper: 95 % Confidence interval
p_value: p-value for the beta listed
n: sample size
varName: Data Field in UKBB
description: description as in the UKBB or has been created for derived variables
ShortName: shorter version of description for plotting purposes
pvalue_CvsF: P-value for the comparison of the Current vs Former results


#We save the results
	
	write.table(RESULTS,"../SupplementaryTables/CurrentFormer_4FEVvariables_LinearResults.txt",sep="\t",quote=TRUE,row.names=FALSE) 

#-->moved this later to ../SupplementaryTables/ST_Final71Results and renamed as g_4FEVCurrentvsFormer.txt

#Doing the same for the forest data. Now as I've already created the Pheno for the RESULTS I'll just get it from there

	P<- RESULTS[,c("varName","Phenotype","Cat3_Title")]
	
	FOREST <- join(F, P, by="varName", type="left")

	#Only keeping wanted columns:

	#> names(FOREST)
	# [1] "workName"     "resType"      "beta"         "se"           "pvalue"
	# [6] "data"         "NotFDRsig"    "varName"      "ending"       "newvarName"
	#[11] "ShortName"    "EverNeversig" "Pheno"        "Cat3_Title"
		
	FOREST <- FOREST[, c("Phenotype", "beta" ,"se", "data", "Cat3_Title", "resType","pvalue","varName")]

	#Ordering by Cat3_Title and Phenotype (not really necessary as the plotting will order them automatically in this order)	
	FOREST <- FOREST[order(FOREST$Cat3_Title,FOREST$Phenotype),]

#We create the following readme_CurrentFormer_4FEVvariables_LinearForest.txt to accompary it (can then copy paste this to another sheet inside the excel)
Phenotype: n = normalised 
beta: coefficient for the GRS from the regression model
se: standard error for beta
data: which dataset, Current/Former
Cat3_Title: The UKBB category
---the below should not be needed for plotting-------
resType: regression model used
pvalue: pvalue for the beta
varName: Data Field in UKBB

#Table for creating the Forest plot
	write.table(FOREST,"./IntermediateData/CurrentFormer_4FEVvariables_LinearForest.txt",sep="\t",quote=TRUE,row.names=FALSE) 



#Then can delete the temporary files we created in bash:
rm ./IntermediateData/Linear_rtnPhenos_ForAnalysis.txt

rm ./IntermediateData/DerivedLinearResults_C.txt
rm ./IntermediateData/DerivedLinearResults_F.txt

rm ./IntermediateData/DerivedLinearForest_C.txt
rm ./IntermediateData/DerivedLinearForest_F.txt













