#6.4.2023
#Checked why the forest plot data was faulty for the coffee quantity (excluding/ including decaf) variables (there were some extra rows)
# cd /[path]
# RDir="/[path]/"
# ${RDir}R-4.1.1/bin/R
# WorkDir="/[path]"
#Looking through the script I noticed the problem arises at the very last stage when we get the Phenotype variable from the RESULTS file and merge with FOREST by varName.
#Ended up simply deleting the extra rows when creating the forestplot (F_FinalForest.R)


#28.3.2023
#Jadwiga Buchwald

#Creating my linear variables (all of them) and running the analyses for ALL/EVER/NEVER
#a) Creating the new variables
#b) Formatting all linear variables like we want them: first we adjust, then we normalise
#c) Running the analyses for ALL/EVER/NEVER 
#d) checking the FDR significance
#e) Merging the ALL/EVER/NEVER results into combined files 
#f) running the ever vs never analysis and checking fdr significance
#g) Getting the Cat3_Title + Description+ ShortName & creating the Pheno variable --> creating our data for the forest plot and for our results table

#---------------------------------------------------------------------------------------

#Doing this interactively

#cd /[path]
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R


#-----------------------------------
#Rscript
#-----------------------------------

#---------------------------------
#Not for Github:

DataDir="/[path]/"

#----------------------------------

sessionInfo()
getwd()
#libraries
	library(plyr)
	library(dplyr)
	library(tidyr)

#----------------------------------------------------------------------------------------
#READING IN THE DATA 


#a) Reading in the phenotype data

	P = read.csv("./IntermediateData/ukb_sorted_n343662_61variables.csv")
	dim(P)


#> dim(P)
#[1] 343662    382

names(P[1:10,1:10])


#b) Reading in trait and confounder data

	#Reading in our GRS data (for the All dataset)
		T = read.csv("../../TRAIT/zGRS10SNPs_topconfig.csv", as.is=T, header=T)

	#we read in the confounder file
		C = read.csv(paste(DataDir,"/ukb_n343662_phesant_confounders.csv",sep=""), as.is=TRUE, header=TRUE)
		names(C)
		str(C)
		dim(C)


#---------------------------------------------------------------------------------------
#a) Creating the new variables
#---------------------------------------------------------------------------------------
#3 new variables:
#1488*		LINEAR	Tea quantity	"""-10 Less than one
# -1 Do not know
# -3 Prefer not to answer""
#"	"-10=0,5| -1=NA|-3=NA|0=NA"	Only includes tea drinkers (black and green), i.e. we excluded those who answered 0.
#1498*		LINEAR	Coffee quantity (including decaf)		"-1=NA|-3=NA|-10=0.5| 0=NA "	Only includes coffee drinkers (including decaf).
#1498*		LINEAR	Coffee quantity 	 	"Excluded those with 1 for variable Coffee type (1508); -1=NA|-3=NA|-10=0.5| 0=NA"	Only includes coffee drinkers and excluded those drinking decaf.


#Note this time we also include the following three variables as continuous:

#b)
#Getting the "old 33 variables (that already had resType=LINEAR)"+ 3 (that had resType=ORDERED-LOGISTIC) that don't need modification:
#The three to be moved from ordinal logistic regression:
#30150*		LINEAR	Eosinophill count			Treated as continuous as opposed to ordered categorical.
#2887*		LINEAR	Number of cigarettes previously smoked daily		"-1=NA|-10=NA"	Treated as continuous as opposed to ordered categorical.
#3456*		LINEAR	Number of cigarettes currently smoked daily (current cigarette smokers)		"-1=NA|-3=NA|-10=NA"	Treated as continuous as opposed to ordered categorical.


#The 33 variables that we already had in the linear regression (and want to keep)
	WS<-read.table("../FDR_initial/IntermediateData/ShortNames61Variables.txt",sep="\t",header=TRUE, quote="\"")
	str(WS)
	dim(subset(WS,WS$resType=="LINEAR")) #33 x 5
	print(subset(WS,WS$resType=="LINEAR")[,c("varName","description")])

#varName                                                       description
#1    30610                                              Alkaline phosphatase
#6    30620                                          Alanine aminotransferase
#9     1807                                             Father's age at death
#11   30700                                                        Creatinine
#12   20154 Forced expiratory volume in 1-second (FEV1), predicted percentage
#13   30530                                                   Sodium in urine
#15   23462                                                           Glycine
#16   23451               Omega-3 Fatty Acids to Total Fatty Acids percentage
#17   30210                                            Eosinophill percentage
#18   23459                  Omega-6 Fatty Acids to Omega-3 Fatty Acids ratio
#19    2897                                               Age stopped smoking
#20   20256               Forced expiratory volume in 1-second (FEV1) Z-score
#22   20150         Forced expiratory volume in 1-second (FEV1), Best measure
#24   30750                                      Glycated haemoglobin (HbA1c)
#25    1488                                                        Tea intake
#28   20258                                           FEV1/ FVC ratio Z-score
#29   25874             Volume of grey matter in Supracalcarine Cortex (left)
#30   30880                                                             Urate
#31   30710                                                C-reactive protein
#33   30630                                                  Apolipoprotein A
#35   23443                                            Degree of Unsaturation
#39    3063                       Forced expiratory volume in 1-second (FEV1)
#40   23449                                                     Linoleic Acid
#41   23109                                          Impedance of arm (right)
#43   26678                                    Volume of VA (left hemisphere)
#44   30680                                                           Calcium
#46      48                                               Waist circumference
#47   30510                                   Creatinine (enzymatic) in urine
#50   23007                      pp150 Nter antigen for Human Cytomegalovirus
#51   20151                         Forced vital capacity (FVC), Best measure
#52   20257                               Forced vital capacity (FVC) Z-score
#57    3062                                       Forced vital capacity (FVC)
# 	"26536"				"Volume-ratio of BrainSegVol-to-eTIV (whole brain)"

#Note x25874_2_0 & x26678_2_0 & x26536_2_0
#Also Note, that I need to fetch x1508_0_0(coffee type) in order to create my variable: d1498_CoffeeQuantity_excldecaf

#----------------------------------------
#Getting all the variables needed:
	#eid, Tea quantity, Coffee quantity,
	#The three from ordinal logistic regression,
	#Coffee type,
	#33 variables that had already been analysed using linear regression.
#----------------------------------------

N<-P[,c("eid", "x1488_0_0", "x1498_0_0",
	"x30150_0_0",  "x2887_0_0"  , "x3456_0_0" , 
	"x1508_0_0",
	"x30610_0_0","x30620_0_0","x1807_0_0","x30700_0_0","x20154_0_0","x30530_0_0","x23462_0_0","x23451_0_0","x30210_0_0","x23459_0_0","x2897_0_0","x20256_0_0","x20150_0_0","x30750_0_0","x1488_0_0","x20258_0_0",
	"x25874_2_0","x30880_0_0","x30710_0_0","x30630_0_0","x23443_0_0","x3063_0_0","x23449_0_0","x23109_0_0","x26678_2_0","x30680_0_0","x48_0_0","x30510_0_0","x23007_0_0","x20151_0_0","x20257_0_0","x3062_0_0","x26536_2_0")]
head(N[,1:10])

	#We modify the tea and coffee variables:
		N$d1488_TeaQuantity<-ifelse(N$x1488_0_0==-10,0.5,N$x1488_0_0)
		N$d1488_TeaQuantity<-ifelse(N$x1488_0_0<=0,NA,N$d1488_TeaQuantity)

	#We create the derived coffee variables:
		N$d1498_CoffeeQuantity_incldecaf<-ifelse(N$x1498_0_0==-10,0.5,N$x1498_0_0)
		N$d1498_CoffeeQuantity_incldecaf<-ifelse(N$x1498_0_0<=0,NA,N$d1498_CoffeeQuantity_incldecaf)

		N$d1498_CoffeeQuantity_excldecaf<-ifelse(N$x1508_0_0==1,-3,NA)
		N$d1498_CoffeeQuantity_excldecaf<-ifelse(is.na(N$d1498_CoffeeQuantity_excldecaf),N$x1498_0_0,N$d1498_CoffeeQuantity_excldecaf)
		N$d1498_CoffeeQuantity_excldecaf<-ifelse(N$d1498_CoffeeQuantity_excldecaf==-10,0.5,N$d1498_CoffeeQuantity_excldecaf)
		N$d1498_CoffeeQuantity_excldecaf<-ifelse(N$d1498_CoffeeQuantity_excldecaf<=0,NA,N$d1498_CoffeeQuantity_excldecaf)

#We delete the "x1508_0_0" from the data for Analysis
#"As well as the original coffee and tea variables: "x1488_0_0", "x1498_0_0"
	A<-N[,-which(names(N) %in% c("x1508_0_0","x1488_0_0", "x1498_0_0"))]


###############################################################################################################

#Then going through all variables: negative values to missing

#Let's have a look at the minimum values each variable gets and that the summary stats look ok:
	str(A)
	summary(A[,2:39])

#The following variables had negative values:  x2887_0_0 CPD previous,  x3456_0_0 CPD current, x1807_0_0 Father's age at death, x2897_0_0 Age stopped smoking ,  
x20256_0_0 FEV1 z-version, x1488_0_0.1 Tea intake, x20258_0_0 FEV1 z-version, x20257_0_0 FEV1 z-version   

	#What is N$x1488_0_0.1?
	table(N$x1488_0_0.1,N$x1488_0_0) #Totally the same as N$x1488_0_0 -->
	#so I'll delete it: 
	A<-N[,-which(names(N) %in% c("x1508_0_0","x1488_0_0", "x1498_0_0", "x1488_0_0.1"))]

	#Quickly checking the coding for the above variables containing negative values (doing this in ukbiobank showcase on their webpage: https://www.ukbiobank.ac.uk/)

	#-->We make negative value smissing for: x2887_0_0 CPD previous,  x3456_0_0 CPD current, x1807_0_0 Father's age at death, x2897_0_0 Age stopped smoking
	#BUT we keep them as they are for: x20256_0_0 FEV1 z-version,  x20258_0_0 FEV1 z-version, x20257_0_0 FEV1 z-version  

#Then for the four variables: x2887_0_0 CPD previous,  x3456_0_0 CPD current, x1807_0_0 Father's age at death, x2897_0_0 Age stopped smoking
#We assign missing values to all negative values:
	
for (i in c("x2887_0_0","x3456_0_0", "x1807_0_0", "x2897_0_0")) { 
	print(i)
	print("before")
	print(summary(A[,which(names(A)==i)]))
	A[,which(names(A)==i)]<-ifelse(A[,which(names(A)==i)]<0,NA,A[,which(names(A)==i)]) 
	print("after")
	print(summary(A[,which(names(A)==i)]))
}


#-------------------------------
#ii) adjusting for confounders and then normalizing.
#-------------------------------

#NOTE this differs from PHESANT: We adjust for covariates FIRST and only THEN normalize the data

#We use the rank based inverse normal transformation instead of just creating z-scores ((x-mean)/sd, which could be done e.g. using the scale function in R)
#PHESANT also uses the rank based inverse normal transformation, and we use the same function as they do:
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

#	#example of use: 
#		rtn_resCPD = irnt(resCPD)
#-----------------------------------------------------------------------------



#1) We adjust for the covariates and then normalize. I.e. we perform inverse normal transformation of the trait residuals

names(A)
str(A)
dim(A) #343662     39


Data<-T
for (i in 2:39) {
	#Creating a merged dataset (exposure+outcome+covariates)
			pname<-names(A)[i]
			p_i<-A[,c(1,i)]
			D <- join(p_i,C, by = "eid", type ="left")
			D<- D[complete.cases(D),]
			#D <- join(D,C, by = "eid", type ="left")
			print(names(D))
			print(dim(D))
	
	#Preparing the data for our analysis
			pheno = D[,pname]
			#exp = D[,"zGRS"]
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


#I'll save these derived variables just in case
write.table(Data,"./IntermediateData/Linear_rtnPhenos_ForAnalysis.txt",sep="\t",quote=F,row.names=F) 


#-----------------------------------------------------------------
#Now we run the analysis using these phenotypes
#------------------------------------------------------------------


#-----------------------function: testLinear----------------------------------------------

#Note: Remember that a function can only return one object! 
#That is why I'm returning the data for the forest plot and for the results table in the same dataframe and then dividing them into two after using the function.

#Reading in eid and the phenos we want to analyse
PhenoTrait<-Data
PhenoTrait<-read.table("./IntermediateData/Linear_rtnPhenos_ForAnalysis.txt", sep="\t",header=TRUE)

#Trait data will define the dataset ALL/EVER/NEVER
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

		#Running the linear regression
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


Reading in the trait data for All/Ever/Never

	#Reading in our GRS data (for the All dataset)
		TA = read.csv("../../TRAIT/zGRS10SNPs_topconfig.csv", as.is=TRUE, header=TRUE)

	#Reading in our GRS data (for the Ever dataset)
		TE = read.csv("../../TRAIT/Ever_zGRS10SNPs_110348.csv", as.is=TRUE, header=TRUE)

	#Reading in our GRS data (for the Never dataset)
		TN = read.csv("../../TRAIT/Never_zGRS10SNPs_135890.csv", as.is=TRUE, header=TRUE)

#For All
	O<-testLinear(dataset="All", Beta="beta_A", Pvalue="pvalue_A", Se="se_A", N="n_A", Lower="lower_A", Upper="upper_A", Phenodata=PhenoTrait[,-2], Traitdata=TA)
	FD_a<-O[,c(1,2,3:6)]
	RD_a<-O[,c(1,2,7:12)]
	FD_a
	RD_a

	write.table(FD_a,"./IntermediateData/DerivedLinearForest_A.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_a,"./IntermediateData/DerivedLinearResults_A.txt",sep="\t",quote=F,row.names=F) 




#For Ever
#Note: have to exclude initiation(9) 
	O<-testLinear(dataset="Ever", Beta="beta_E", Pvalue="pvalue_E", Se="se_E", N="n_E", Lower="lower_E", Upper="upper_E", Phenodata=PhenoTrait[,-2], Traitdata=TE)
	FD_e<-O[,c(1,2,3:6)]
	RD_e<-O[,c(1,2,7:12)]
	FD_e
	RD_e

	write.table(FD_e,"./IntermediateData/DerivedLinearForest_E.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_e,"./IntermediateData/DerivedLinearResults_E.txt",sep="\t",quote=F,row.names=F) 


#For Never
#Note: have to exclude: 
#2887*		LINEAR	Number of cigarettes previously smoked daily		"-1=NA|-10=NA"	Treated as continuous as opposed to ordered categorical.
#3456*		LINEAR	Number of cigarettes currently smoked daily (current cigarette smokers)		"-1=NA|-3=NA|-10=NA"	Treated as continuous as opposed to ordered categorical.
#19    2897                                               Age stopped smoking


exclude<-which(names(PhenoTrait) %in% c("zGRS","nx2887_0_0", "nx3456_0_0", "nx2897_0_0"))

	O<-testLinear(dataset="Never", Beta="beta_N", Pvalue="pvalue_N", Se="se_N", N="n_N", Lower="lower_N", Upper="upper_N", Phenodata=PhenoTrait[,-exclude], Traitdata=TN)
	FD_n<-O[,c(1,2,3:6)]
	RD_n<-O[,c(1,2,7:12)]
	FD_n
	RD_n

	write.table(FD_n,"./IntermediateData/DerivedLinearForest_N.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_n,"./IntermediateData/DerivedLinearResults_N.txt",sep="\t",quote=F,row.names=F) 

#----------------------------------------

#######################################################################################################################################
#d) checking the FDR significance
#######################################################################################################################################
#I will create the following variables: 
	#FDRsig (for the results table), 
	#NotFDRsig (for the forest plot), 
	#pvalue_EvsN, 
	#EverNeverSig (for results table and for creating Pheno)

	FD_a<-read.table("./IntermediateData/DerivedLinearForest_A.txt", sep="\t",header=TRUE)
	FD_e<-read.table("./IntermediateData/DerivedLinearForest_E.txt", sep="\t",header=TRUE)
	FD_n<-read.table("./IntermediateData/DerivedLinearForest_N.txt", sep="\t",header=TRUE)

	RD_a<-read.table("./IntermediateData/DerivedLinearResults_A.txt", sep="\t",header=TRUE)
	RD_e<-read.table("./IntermediateData/DerivedLinearResults_E.txt", sep="\t",header=TRUE)
	RD_n<-read.table("./IntermediateData/DerivedLinearResults_N.txt", sep="\t",header=TRUE)

	dim(FD_a); dim(FD_e); dim(FD_n) #38 x 6 for all and ever and 35 x 6 for never
	dim(RD_a); dim(RD_e); dim(RD_n) #38 x 8 for all and ever and 35 x 8 for never

	#checking the FDR significance (thresholds from: ../FDR_initial/Scripts/B_FDRthreshold.R)
		#ALL

			threshold_ALL <- 0.05*47/21094

			RD_a$FDRsig_A <- ifelse(RD_a$pvalue_A < threshold_ALL,1,0)
			dim(subset(RD_a, RD_a$FDRsig_A==1)) #30 x 9
		
			FD_a$NotFDRsig <- ifelse(FD_a$pvalue < threshold_ALL,0,1)
			dim(subset(FD_a, FD_a$NotFDRsig==0)) #30 x 7

		#EVER
			threshold_EVER <- 0.05*29/16648
			RD_e$FDRsig_E <- ifelse(RD_e$pvalue_E < threshold_EVER,1,0)
			dim(subset(RD_e, RD_e$FDRsig_E==1)) #13 x 9
		
			FD_e$NotFDRsig <- ifelse(FD_e$pvalue < threshold_EVER,0,1)
			dim(subset(FD_e, FD_e$NotFDRsig==0)) #13 x 7


		#NEVER
			threshold_NEVER <- 0.05*2/16103
			RD_n$FDRsig_N <- ifelse(RD_n$pvalue_N < threshold_NEVER,1,0)
			dim(subset(RD_n, RD_n$FDRsig_N==1)) #3 x 9
		
			FD_n$NotFDRsig <- ifelse(FD_n$pvalue < threshold_NEVER,0,1)
			dim(subset(FD_n, FD_n$NotFDRsig==0)) #3 x 7


#######################################################################################################################################
#e) Merging the ALL/EVER/NEVER results into combined files 
#######################################################################################################################################
#Then we a) concatenate the three FD (Forestplot data) datasets by workName 
#And b) merge the three RD (Results table data) datasets 

#Forest:
	Forest<-rbind(FD_a,FD_e,FD_n)
	F<-Forest[order(Forest$workName),]

	F$varNamelong<-substring(F$workName,3) #starting from the third character we copy workName to varNamelong, i.e we drop the nx and nd.
	F<-separate(F, varNamelong, into = c("varName", "ending"), sep = "_", extra="merge") #So I can get Cat3 and ShortName from another file
	F$newvarName<-paste("n",F$varName,sep="") #Will have to add d to the first nine rows(the three tea/coffee quanitity variables)
	F$newvarName[1:9]<-paste("nd",F$varName[1:9],sep="")
	F$ShortName<-NA
	F$ShortName[1:9]<-F$ending[1:9] #will clean these up by hand. newvarName and ShortName will be used to create variable Pheno for plotting. 	


#Results:
	names(RD_a)
	#We join the results (we delete resType from the ever and never versions not to have that multiple times)
	Results<-join(RD_a, RD_e[,-2], by="workName", type="left")
	Results<-join(Results, RD_n[,-2], by="workName", type="left")

	Results$varNamelong<-substring(Results$workName,3) #starting from the third character we copy workName to varNamelong, i.e we drop the nx and nd.
	Results<-separate(Results, varNamelong, into = c("varName", "ending"), sep = "_", extra="merge") #So I can get Cat3 and ShortName from another file
	Results$newvarName<-paste("n",Results$varName,sep="") #Will have to add d to the last 3 rows(the three tea/coffee quanitity variables)
	Results$newvarName[36:38]<-paste("nd",Results$varName[36:38],sep="")
	Results$ShortName<-NA
	Results$ShortName[36:38]<-Results$ending[36:38] #will clean these up by hand. newvarName and ShortName will be used to create variable Pheno for plotting. 	

#Out of curiosity checking how many variables of the 38 have a FDR significant result for ALL, EVER and/or NEVER.
	Results$FDRsigSum<-Results$FDRsig_A+Results$FDRsig_E+Results$FDRsig_N
	Results$FDRsigSum <- rowSums(Results[,c("FDRsig_A", "FDRsig_E","FDRsig_N")], na.rm=TRUE)
	table(Results$FDRsigSum)
	
 	#0  1  2  3
 	#5 22  9  2	
	#--> 33 of the variables have a FDRsig result in at least one subset of data



#######################################################################################################################################
#f) running the ever vs never analysis and checking fdr significance
#######################################################################################################################################

		threshold_EvsN <- 0.05*2/15212
		Results$x = (Results$beta_E-Results$beta_N)^2 / (Results$se_E^2+Results$se_N^2)
      		Results$pvalue_EvsN = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) #38 x 29

		Results$EverNeversig <- ifelse(Results$pvalue_EvsN < threshold_EvsN,1,0)
		dim(subset(Results, Results$EverNeversig==1)) #4 x 30

		#Adding EverNeverSig to the Forest data
		list <- subset(Results, Results$EverNeversig==1)[,"workName"]
		F$EverNeversig<-ifelse(F$workName %in% list, 1, 0)
		subset(F, F$EverNeversig==1)

#######################################################################################################################################
g) Getting the Cat3_Title + Description+ ShortName & creating the Pheno variable --> creating our data for the forest plot and for our results table
#######################################################################################################################################

	ShortNames<-read.table("../FDR_initial/IntermediateData/ShortNames61Variables.txt",sep="\t",quote="\"",header=TRUE)

#We get Cat3_Title for all variables
	#We delete "resType"     "description" "ShortName"  from ShortNames before merging (for now)
		RESULTS<-join(Results, ShortNames[,-c(2,3,4)], by="varName", type="left")

#We only get the ShortName and description for those that don't yet have ShortName (all but last three variables). 
	RESULTSa <- RESULTS[1:35,-which(names(RESULTS)=="ShortName")]
	RESULTSb <- RESULTS[36:38,]
	RESULTSa <- join(RESULTSa, ShortNames[,-c(2,5)], by="varName", type="left")

#Modifying the ShortName for the three last variables into the same format as for others:
	#RESULTSb$ShortName
	#[1] "TeaQuantity"              "CoffeeQuantity_incldecaf"
	#[3] "CoffeeQuantity_excldecaf"

	RESULTSb$ShortName <- c("Tea quantity", "Coffee quantity (including decaf)", "Coffee quantity (excluding decaf)")
	RESULTSb$description <- RESULTSb$ShortName

#We create Pheno 
	RESULTSb$Pheno <- paste(RESULTSb$newvarName, RESULTSb$ShortName, sep=" - ")
	RESULTSa$Pheno <- paste(RESULTSa$newvarName, RESULTSa$ShortName, sep=" - ")
	

#Only keeping wanted columns and then we rbind the two sets of data
> names(RESULTSa)
 [1] "workName"     "resType"      "beta_A"       "lower_A"      "upper_A"
 [6] "se_A"         "pvalue_A"     "n_A"          "FDRsig_A"     "beta_E"
[11] "lower_E"      "upper_E"      "se_E"         "pvalue_E"     "n_E"
[16] "FDRsig_E"     "beta_N"       "lower_N"      "upper_N"      "se_N"
[21] "pvalue_N"     "n_N"          "FDRsig_N"     "varName"      "ending"
[26] "newvarName"   "FDRsigSum"    "x"            "pvalue_EvsN"  "EverNeversig"
[31] "Cat3_Title"   "description"  "ShortName"    "Pheno"

	RESULTSa <- RESULTSa[,c("Pheno", "resType", "Cat3_Title", 
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", "FDRsig_A",
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", "FDRsig_E",
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", "FDRsig_N",
				"varName", "description", "ShortName", "FDRsigSum","pvalue_EvsN", "EverNeversig")]
	RESULTSb <- RESULTSb[,c("Pheno", "resType", "Cat3_Title", 
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", "FDRsig_A",
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", "FDRsig_E",
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", "FDRsig_N",
				"varName", "description", "ShortName", "FDRsigSum","pvalue_EvsN", "EverNeversig")]

	RESULTS<-rbind(RESULTSa,RESULTSb)	



#Ordering based on Cat3_Title and Pheno
	RESULTS <- RESULTS[order(RESULTS$Cat3_Title,RESULTS$Pheno),]

#Adding a * to the beginning of Pheno when EverNeversig=1
	RESULTS$Phenotype <- ifelse(RESULTS$EverNeversig==1, paste("*",RESULTS$Pheno,sep=""), RESULTS$Pheno)
	RESULTS$Phenotype <- ifelse(is.na(RESULTS$Phenotype), RESULTS$Pheno, RESULTS$Phenotype)

#We delete the Pheno and instead keep Phenotype and move it to be the first variable
	RESULTS <- RESULTS[,c("Phenotype", "resType", "Cat3_Title", 
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", "FDRsig_A",
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", "FDRsig_E",
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", "FDRsig_N",
				"varName", "description", "ShortName", "FDRsigSum","pvalue_EvsN", "EverNeversig")]


#We save the results file as a supplementary table
#We create the following readme_Final_38_LinearResults.txt to accompary it (can then copy paste this to another sheet inside the excel)
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
	#Results table that has to be concatenated with the binary and Orderedlogistic ones. 
	write.table(RESULTS,"./IntermediateData/Final_38_LinearResults.txt",sep="\t",quote=TRUE,row.names=FALSE) 
	write.table(RESULTS,"./IntermediateData/Final_38_LinearResults.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)


#Doing the same for the forest data. Now as I've already created the Pheno for the RESULTS I'll just get it from there

	P<- RESULTS[,c("varName","Phenotype","Cat3_Title")]
	
	FOREST <- join(F, P, by="varName", type="left")

	#Only keeping wanted columns:

	#> names(FOREST)
	# [1] "workName"     "resType"      "beta"         "se"           "pvalue"
	# [6] "data"         "NotFDRsig"    "varName"      "ending"       "newvarName"
	#[11] "ShortName"    "EverNeversig" "Pheno"        "Cat3_Title"
		
	FOREST <- FOREST[, c("Phenotype", "beta" ,"se", "NotFDRsig", "data", "Cat3_Title", "resType","pvalue","varName")]

	#Ordering by Cat3_Title and Phenotype (not really necessary as the plotting will order them automatically in this order)	
	FOREST <- FOREST[order(FOREST$Cat3_Title,FOREST$Phenotype),]

#We create the following readme_Final_38_LinearForForest.txt to accompary it (can then copy paste this to another sheet inside the excel)
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
	write.table(FOREST,"./IntermediateData/Final_38_LinearForest.txt",sep="\t",quote=TRUE,row.names=FALSE) 
	write.table(FOREST,"./IntermediateData/Final_38_LinearForest.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)


#Then can delete the temporary files we created in bash:
rm ./IntermediateData/Linear_rtnPhenos_ForAnalysis.txt

rm ./IntermediateData/DerivedLinearResults_A.txt
rm ./IntermediateData/DerivedLinearResults_E.txt
rm ./IntermediateData/DerivedLinearResults_N.txt

rm ./IntermediateData/DerivedLinearForest_A.txt
rm ./IntermediateData/DerivedLinearForest_E.txt
rm ./IntermediateData/DerivedLinearForest_N.txt


