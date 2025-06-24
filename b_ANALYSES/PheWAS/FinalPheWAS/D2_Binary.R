#8.8.2022
#Jadwiga Buchwald

#I) I'll start by checking I've understood the dentures variable and get the same results as phesant gave
#II) Creating all the new binary variables for the final analyses

#Structure of this document
#1. reading in the data needed for both I and II
#2. creating the new variables for I and doing the analyses
#3. creating the new variables for II and doing the analyses + saving the forest plot formatted data and the results package formatted data

#---------------------------------------------------------------------------------------

#DOing this interactively

#cd /[path]
#RDir="/[path]/"
#${RDir}R-4.1.1/bin/R

#For combining derived with old I was in my home directory
#cd /[path]

#-----------------------------------
#Rscript
#-----------------------------------

#---------------------------------
#Not for Github:

DataDir="/[path]/"

TraitDir="/[path]/"

#For combining derived with old:
WorkDir="/[path]"

#----------------------------------


sessionInfo()
getwd()
#libraries
	library(plyr)
	library(dplyr)
	library(tidyr)

#----------------------------------------------------------------------------------------
#READING IN THE DATA NEEDED FOR BOTH I) AND II)


#a) Reading in the phenotype data

	P = read.csv("./IntermediateData/ukb_sorted_n343662_61variables.csv")
	dim(P)

#> dim(P)
#[1] 343662    415


#b) Reading in trait and confounder data

	#Reading in our GRS data (for the All dataset)
		T = read.csv("../../TRAIT/zGRS10SNPs_topconfig.csv", as.is=T, header=T)
		dim(T) #343662      2

	#we read in the confounder file
		C = read.csv(paste(DataDir,"/ukb_n343662_phesant_confounders.csv",sep=""), as.is=TRUE, header=TRUE)
		names(C)
		str(C)
		dim(C)

#---------------------------------------------------------------------------------------

#I) Sanity check: dentures

#The two variables I'll check: 

#a)  6149#100 CAT-MUL Mouth/teeth dental problems

	#1	Mouth ulcers
	#2	Painful gums
	#3	Bleeding gums
	#4	Loose teeth
	#5	Toothache
	#6	Dentures
	#-7	None of the above
	#-3	Prefer not to answer

	#clearly -7=100 and -7 was none of the above so 6149#100 is a 0-1 binary variable: free of mouth/dental problems


# b) 6149#6 CAT-MUL Mouth/teeth dental problems: Dentures
#x6149_0_0-x6149_0_5

#Same coding as above

#------------------------------------------------------
str(P)

#Creating the variables and checking the n
#Just keeping x6149_0_0-x6149_0_5 and eid for our sanity check

	S<-P[,c("eid","x6149_0_0","x6149_0_1","x6149_0_2","x6149_0_3","x6149_0_4","x6149_0_5")]

	table(S$x6149_0_0)
	#    -7     -3      1      2      3      4      5      6
	#208402    743  34997   6389  34325   7492   6376  44616

	table(S$x6149_0_1)
	#   2    3    4    5    6
	#3104 8489 4403 4510 9892


#Correcting coding, i.e. creating the "no tooth/mouth problems" and "dentures" variables
	#Creating the none of the above version i.e. should=6149#100
		S$noprob<-ifelse(S$x6149_0_0==-3,NA,S$x6149_0_0)
		S$noprob<-ifelse(S$noprob==-7,1,0)
			table(S$noprob)
			#  0      1
			#134195 208402
#134195+208402 #342597

#-->We get the exact same n numbers as phesant outputs for 6149#100


	#Creating the dentures version i.e. should = 6149#6
		#First replacing missing values so we don't loose most of our data when we go through S$x6149_0_1 to S$x6149_0_5
			S[is.na(S)] <- -3

		#Dealing with S$x6149_0_0
			S$dentures<-ifelse(S$x6149_0_0==-3,NA,S$x6149_0_0)
				table(S$dentures)
			S$dentures<-ifelse(S$dentures<6,0,1)
				table(S$dentures)
		#Dealing with S$x6149_0_1 to S$x6149_0_5
			S$dentures<-ifelse(S$x6149_0_1==6 | S$x6149_0_2==6| S$x6149_0_3==6| S$x6149_0_4==6| S$x6149_0_5==6,1,S$dentures)		
				table(S$dentures)
				#     0      1
				#284781  57816

#-->We get the exact same n numbers as phesant outputs for 6149#6

#-----------------------------------------------------
#Doing the analyses and checking we get the same results as phesant gives

#PHESANT output
#ALL:
#"varName","varType","n","beta","lower","upper","pvalue","resType","description"
#6149#100	Categorical multiple	134195/208402(342597)	-0.0157375276563368	-0.022653144073221	-0.00882452595859026	8.15586044228975e-06	LOGISTIC-BINARY	Mouth/teeth dental problems
#6149#6	Categorical multiple	284781/57816(342597)	0.0147316440223939	0.0053590951538646	0.0241152587332548	0.00207804100972336	LOGISTIC-BINARY	Mouth/teeth dental problems: Dentures

#Combining the data using eid
names(S)
Sn<-S[,c("eid", "noprob")]
Sd<-S[,c("eid", "dentures")]

		Dn <- join(T,Sn, by = "eid", type ="left")
		Dn <- join(Dn,C, by = "eid", type ="left")
		names(Dn)
		dim(Dn)

		Dd <- join(T,Sd, by = "eid", type ="left")
		Dd <- join(Dd,C, by = "eid", type ="left")
		names(Dd)
		dim(Dd)

		#We delete the column eid:  Dn[,-1] so that it is not used as a confounder
		mylogit_n <- glm(noprob ~ zGRS + ., data=Dn[,-1], family="binomial")
			
                sumx = summary(mylogit_n)
                pvalue = sumx$coefficients['zGRS','Pr(>|z|)']
                beta = sumx$coefficients["zGRS","Estimate"]
                cis = confint(mylogit_n, "zGRS", level=0.95)
                lower = cis["2.5 %"]
                upper = cis["97.5 %"]

		results_n<- data.frame(beta, lower, upper, pvalue)
			#> results_n
             		#beta       lower        upper      pvalue
			# -0.01573753 -0.02265314 -0.008824526 8.15586e-06
	#-->Exact same results as PHESANT gives

		mylogit_d <- glm(dentures ~ zGRS + ., data=Dd[,-1], family="binomial")

 		sumx = summary(mylogit_d)
                pvalue = sumx$coefficients['zGRS','Pr(>|z|)']
                beta = sumx$coefficients["zGRS","Estimate"]
                cis = confint(mylogit_d, "zGRS", level=0.95)
                lower = cis["2.5 %"]
                upper = cis["97.5 %"]
		
		results_d<- data.frame(beta, lower, upper, pvalue)
			#> results_d
            		#beta       lower      upper      pvalue
			#0.01473164 0.005359095 0.02411526 0.002078041
	#-->Exact same results as PHESANT gives

#So I managed to check that I get the same results as phesant gives i.e. I've understood these variables correctly.

#---------------------------------------------------------------------------------------
#II) Creating the new binary variables

#varName	varType	resType	description
#1 1488*		LOGISTIC-BINARY	Tea drinker
#2 20116*		LOGISTIC-BINARY	Cessation
#3 20116*		LOGISTIC-BINARY	Initiation

#4 1508*		LOGISTIC-BINARY	Decaf vs no coffee
#5 1508*		LOGISTIC-BINARY	Instant vs no coffee
#6 1508*		LOGISTIC-BINARY	Ground vs no coffee
#7 1508*		LOGISTIC-BINARY	Other coffee vs no coffee

#8 1508*		LOGISTIC-BINARY	Decaf vs caffeinated coffee
#9 1508*		LOGISTIC-BINARY	Instant vs other caffeinated coffee

#10 22506*		LOGISTIC-BINARY	Occasional smoker
#11 1498*		LOGISTIC-BINARY	Coffee drinker (including decaf)
#12 1498*		LOGISTIC-BINARY	Coffee drinker (excluding decaf)


names(P)

	#We need the following variables:
	# "x1488_0_0",  "x20116_0_0"  , "x1508_0_0" , "x22506_0_0", "x1498_0_0" 
 
	#We select them and eid:
		N<-P[,c("eid","x1488_0_0",  "x20116_0_0"  , "x1508_0_0" , "x22506_0_0", "x1498_0_0" )]

	#We remove P from our environment from taking up space/memory
		rm(P)

###########################################################
#Creating the new versions
###########################################################

#----------------------------------------------------------------
#1 1488*		LOGISTIC-BINARY	Tea drinker
#Whether or not a person drinks tea

	#1488:
	#"-10 Less than one
	# -1 Do not know
	# -3 Prefer not to answer"
	#> str(N$x1488_0_0)
	# int [1:343662] 1 1 0 0 6 0 2 0 2 4 ...
	#
	#> summary(N$x1488_0_0)
	#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
	#-10.000   1.000   3.000   3.152   5.000  81.000

#Recoding needed: "-10=1| -1=NA|-3=NA|> 0 =1| 0=0"
#----------------------------------------------------------------

		N$x1488der_TeaDrinker<-ifelse(N$x1488_0_0==-10,1,N$x1488_0_0)
		N$x1488der_TeaDrinker<-ifelse(N$x1488der_TeaDrinker<0,NA,N$x1488der_TeaDrinker)
		N$x1488der_TeaDrinker<-ifelse(N$x1488der_TeaDrinker>0,1,N$x1488der_TeaDrinker)
		
		table(N$x1488der_TeaDrinker)			
		#     0      1
		# 50676 292337

#----------------------------------------------------------------
#2 20116*		LOGISTIC-BINARY	Cessation
#Among Ever smokers, Has succeeded to quit: Yes=1, No=0

	#20116:
	#coding meaning
	#-3 Prefer not to answer
	#0 Never
	#1 Previous
	#2 Current

#Recoding needed: "1=1| 2=0| -3=NA | 0=NA"
#----------------------------------------------------------------

		N$x20116der_Cessation<-ifelse(N$x20116_0_0<=0,NA,N$x20116_0_0)
		N$x20116der_Cessation<-ifelse(N$x20116der_Cessation==2,0,N$x20116der_Cessation)
		N$x20116der_Cessation<-ifelse(N$x20116der_Cessation==1,1,N$x20116der_Cessation)
		
		table(N$x20116der_Cessation)	
		#     0      1
		# 34610 120786


		table(N$x20116_0_0)
		#    -3      0      1      2
		#  1188 187078 120786  34610

#----------------------------------------------------------------
#3 20116*		LOGISTIC-BINARY	Initiation
#Has initiated smoking: Yes=1 (Current and previous smokers), No=0 (Never smokers).

#Recoding needed: "0=0| 1=1| 2=1| -3=NA"
#----------------------------------------------------------------

		N$x20116der_Initiation<-ifelse(N$x20116_0_0==-3,NA,N$x20116_0_0)
		N$x20116der_Initiation<-ifelse(N$x20116der_Initiation==2,1,N$x20116der_Initiation)
		
		table(N$x20116der_Initiation)
		#     0      1
		#187078 155396

#----------------------------------------------------------------
#Coffee types - those who don't drink coffee as the reference group

#4 1508*		LOGISTIC-BINARY	Coffee type - Decaffeinated coffee
#Drinks decaffeinated coffee as opposed to not drinking coffee at all: Yes=1, No=0.

#5 1508*		LOGISTIC-BINARY	Coffee type - Instant coffee
#Drinks instant coffee as opposed to not drinking coffee at all: Yes=1, No=0.

#6 1508*		LOGISTIC-BINARY	Coffee type - Ground coffee
#7 1508*		LOGISTIC-BINARY	Coffee type - Other coffee
	

	#1508:
	#coding meaning
	#-3 Prefer not to answer
	#-1 Do not know
	#1 Decaffeinated coffee (any type)
	#2 Instant coffee
	#3 Ground coffee (include espresso, filter etc)
	#4 Other type of coffee

	#table(N$x1508_0_0)	
	# -3     -1      1      2      3      4
   	#238    726  53517 152324  60256   4524

#Those with 0 for variable Coffee intake (1498) coded as 0" 

	summary(N$x1498_0_0)
		#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
		#-10.000   0.000   2.000   1.332   3.000  80.000
	dim(subset(N,N$x1498_0_0==0)) #71266     9

#--> i.e. for these ones (with those not drinking coffee as the reference group) I should always have n=71266  zero values

#----------------------------------------------------------------

#Creating:
#4. x1508der_Decafvsnone
#5. x1508der_Instvsnone
#6. x1508der_Groundvsnone
#7. x1508der_Othervsnone


		N$x1508der_Decafvsnone<-ifelse(N$x1508_0_0==1,1,NA)
		table(N$x1508der_Decafvsnone)
		N$x1508der_Decafvsnone<-ifelse(N$x1498_0_0==0,0,N$x1508der_Decafvsnone)
		table(N$x1508der_Decafvsnone)
			#    0     1
			#71266 53517


		N$x1508der_Instvsnone<-ifelse(N$x1508_0_0==2,1,NA)
		table(N$x1508der_Instvsnone)
		N$x1508der_Instvsnone<-ifelse(N$x1498_0_0==0,0,N$x1508der_Instvsnone)
		table(N$x1508der_Instvsnone)
			#0      1
			#71266 152324
		
		N$x1508der_Groundvsnone<-ifelse(N$x1508_0_0==3,1,NA)
		table(N$x1508der_Groundvsnone)
		N$x1508der_Groundvsnone<-ifelse(N$x1498_0_0==0,0,N$x1508der_Groundvsnone)
		table(N$x1508der_Groundvsnone)
			#    0     1
			#71266 60256

		N$x1508der_Othervsnone<-ifelse(N$x1508_0_0==4,1,NA)
		table(N$x1508der_Othervsnone)
		N$x1508der_Othervsnone<-ifelse(N$x1498_0_0==0,0,N$x1508der_Othervsnone)
		table(N$x1508der_Othervsnone)
			#    0     1
			#71266  4524

#----------------------------------------------------------------
#Coffee types: comparing within coffee drinkers

#Also creating the following coffee variables as these might infact be easier to interpret than when the reference group is "non-coffee drinkers":
#8 1508*		LOGISTIC-BINARY	Decaf vs caffeinated coffee
#9 1508*		LOGISTIC-BINARY	Instant vs other caffeinated coffee


#Then creating the ones I now feel might be more relevant 
#1. x1508der_DecafvsCaf
#2. x1508der_InstvsOtherCaf

#1508:
	#coding meaning
	#-3 Prefer not to answer
	#-1 Do not know
	#1 Decaffeinated coffee (any type)
	#2 Instant coffee
	#3 Ground coffee (include espresso, filter etc)
	#4 Other type of coffee
	#table(N$x1508_0_0)	
		# -3     -1      1      2      3      4
   		#238    726  53517 152324  60256   4524

	#Caf: 152324+60256+4524 #217104
	#Inst + Ground: 152324+ 60256 #212580


		N$x1508der_DecafvsCaf<-ifelse(N$x1508_0_0<0,NA,N$x1508_0_0)
		table(N$x1508der_DecafvsCaf)
		N$x1508der_DecafvsCaf<-ifelse(N$x1508der_DecafvsCaf>=2,0,N$x1508der_DecafvsCaf)
		table(N$x1508der_DecafvsCaf) 
			#     0      1
			#217104  53517


		N$x1508der_InstvsOtherCaf<-ifelse(N$x1508_0_0<=1,NA,N$x1508_0_0)
		table(N$x1508der_InstvsOtherCaf)
		N$x1508der_InstvsOtherCaf<-ifelse(N$x1508der_InstvsOtherCaf==2,1,N$x1508der_InstvsOtherCaf)
		N$x1508der_InstvsOtherCaf<-ifelse(N$x1508der_InstvsOtherCaf >= 3,0,N$x1508der_InstvsOtherCaf)
		table(N$x1508der_InstvsOtherCaf)
			#     0      1
			# 64780 152324
		
		

#----------------------------------------------------------
#8 22506*		LOGISTIC-BINARY	Occasional smoker
#Only smokes occasionally instead of daily: Yes=1, No=0.

#22506_0_0 Tobacco smoking
	#coding meaning
	#-818 Prefer not to answer
	#111 Smokes on most or all days
	#112 Occasionally
	#113 Ex-smoker
	#114 Never smoked

#Recoding needed: "111 = 0 |112 = 1 | -818=NA| 113=NA | 114=NA"
#----------------------------------------------------------
	table(N$x22506_0_0)
		# -818   111   112   113   114
		#  425  1990  1278 31663 52310

	N$x22506der_OccasionalvsDaily<-ifelse(N$x22506_0_0==112,1,NA)
	N$x22506der_OccasionalvsDaily<-ifelse(N$x22506_0_0==111,0,N$x22506der_OccasionalvsDaily)
	table(N$x22506der_OccasionalvsDaily)
		#   0    1
		# 1990 1278

#-------------------------------------------------------------------------
#9 1498*		LOGISTIC-BINARY	Coffee drinker (including decaf)
#Whether or not a person drinks coffee (including decaf): Yes=1, No=0
#"-10=1| -1=NA|-3=NA|> 0 =1| 0=0"

#10 1498*		LOGISTIC-BINARY	Coffee drinker (excluding decaf)
#Whether or not a person drinks coffee: Yes=1, No=0
#"Excluded those with 1 for variable Coffee type (1508); -10=1| -1=NA|-3=NA|> 0 =1| 0=0"
#-------------------------------------------------------------------------

#1498_0_0 Coffee intake: "-1=NA|-3=NA|-10=NA"

	summary(N$x1498_0_0)
		#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
		#-10.000   0.000   2.000   1.332   3.000  80.000
	dim(subset(N,N$x1498_0_0==0)) #71266     9


	N$d1498_CoffeeDrinker_incldecaf<-ifelse(N$x1498_0_0>0 | N$x1498_0_0==-10, 1, N$x1498_0_0)
	N$d1498_CoffeeDrinker_incldecaf<-ifelse(N$d1498_CoffeeDrinker_incldecaf<0, NA, N$d1498_CoffeeDrinker_incldecaf)
	table(N$d1498_CoffeeDrinker_incldecaf)
		#     0      1
		# 71266 271843

	test<-ifelse(N$x1498_0_0>0, 1, N$x1498_0_0)
	table(test)
		#  -10     -3     -1      0      1
		# 24369     79    474  71266 247474
	247474+24369 #271843


	#In order NOT to loose those with 0 for 1498 (not included in x1508_0_0) we use else=-3 and not N$x1498_0_0 in the first statement:

	N$d1498_CoffeeDrinker_excldecaf<-ifelse(N$x1508_0_0==1,-3,NA)
	N$d1498_CoffeeDrinker_excldecaf<-ifelse(is.na(N$d1498_CoffeeDrinker_excldecaf),N$x1498_0_0,N$d1498_CoffeeDrinker_excldecaf)
	N$d1498_CoffeeDrinker_excldecaf<-ifelse(N$d1498_CoffeeDrinker_excldecaf>0 | N$d1498_CoffeeDrinker_excldecaf==-10 ,1,N$d1498_CoffeeDrinker_excldecaf)
	N$d1498_CoffeeDrinker_excldecaf<-ifelse(N$d1498_CoffeeDrinker_excldecaf<0 ,NA,N$d1498_CoffeeDrinker_excldecaf)
	table(N$d1498_CoffeeDrinker_excldecaf)
		#     0      1
		# 71266 218326

#Note, I could have alternatively coded decaf drinkers as 0 i.e. non cafeinated coffee drinkers. However now they've just been dropped out.


#Now that we have created all our derived phenotypes I'll save the data

	#First I'll rename the variables:
	names(N)[7:18]
	names(N)[names(N)=="x1488der_TeaDrinker"]<-"d1488_TeaDrinker"
	names(N)[names(N)=="x20116der_Cessation"]<-"d20116_Cessation"
	names(N)[names(N)=="x20116der_Initiation"]<-"d20116_Initiation"
	names(N)[names(N)=="x1508der_Decafvsnone"]<-"d1508_Decafvsnone"
	names(N)[names(N)=="x1508der_Instvsnone"]<-"d1508_Instvsnone"
	names(N)[names(N)=="x1508der_Groundvsnone"]<-"d1508_Groundvsnone"
	names(N)[names(N)=="x1508der_Othervsnone"]<-"d1508_Othervsnone"
	names(N)[names(N)=="x1508der_DecafvsCaf"]<-"d1508_DecafvsCaf"
	names(N)[names(N)=="x1508der_InstvsOtherCaf"]<-"d1508_InstvsOtherCaf"
	names(N)[names(N)=="x22506der_OccasionalvsDaily"]<-"d22506_OccasionalvsDaily"

write.table(N,"./IntermediateData/DerivedBinaryPhenos.txt",sep="\t",quote=F,row.names=F) 


#####################################################################################################################################################
#####################################################################################################################################################


#Then running the logistic regression for the new derived phenotypes! 
#I created the function testBinary which is an adaptation of the PHESANT one: 
#https://github.com/MRCIEU/PHESANT-MR-pheWAS-smoking/blob/master/2-PHESANT/PHESANT-from-saved/testBinary.r


a) Reading in Phenodata and confounder data

	DP<-read.table(paste(WorkDir,"/FDR_final/IntermediateData/DerivedBinaryPhenos.txt",sep=""), sep="\t",header=TRUE)
	dim(DP)	 # 343662     18

	#we read in the confounder file
		
		C = read.csv(paste(DataDir,"/ukb_n343662_phesant_confounders.csv",sep=""), as.is=TRUE, header=TRUE)
		names(C)
		str(C)
		dim(C)

#b) Reading in trait data for All/Ever/Never

	#Reading in our GRS data (for the All dataset)
		TA = read.csv(paste(TraitDir,"zGRS10SNPs_topconfig.csv",sep=""), as.is=TRUE, header=TRUE)

	#Reading in our GRS data (for the Ever dataset)
		TE = read.csv(paste(TraitDir,"Ever_zGRS10SNPs_110348.csv",sep=""), as.is=TRUE, header=TRUE)

	#Reading in our GRS data (for the Never dataset)
		TN = read.csv(paste(TraitDir,"Never_zGRS10SNPs_135890.csv",sep=""), as.is=TRUE, header=TRUE)

	
#-----------------------function: testBinary----------------------------------------------

#Note: Remember that a function can only return one object! 
#That is why I'm returning the data for the forest plot and for the results table in the same dataframe and then dividing them into two after using the function.

testBinary<-function(dataset, Beta, Pvalue, Se, N, Lower, Upper, Phenodata, Traitdata, Covariatedata){
	
	OUT<-data.frame()
	
	for (i in colnames(Phenodata[,-1])) {

		workName<-i
		resType<-"LOGISTIC-BINARY"
		data=dataset

		#Creating a merged dataset (exposure+outcome+covariates)
			p_i<-Phenodata[,c("eid", i)]
			D <- join(Traitdata,p_i, by = "eid", type ="left")
			D <- join(D,Covariatedata, by = "eid", type ="left")
			names(D)
			dim(D)

		#Preparing the data for our analysis
			pheno = D[,i]
			phenoFactor = factor(pheno)
			exp = D[,"zGRS"]
			confNames = colnames(Covariatedata)
        		confNames = confNames[-which(confNames=="eid")]
			confs = D[,confNames]

		#Getting the n
			facLevels = levels(phenoFactor)
			num0 = length(which(phenoFactor==facLevels[1]))
			num1 = length(which(phenoFactor==facLevels[2]))
			numNotNA = length(which(!is.na(phenoFactor)))
			n<-paste(num0,"/",num1,"(",numNotNA,")",sep="")

		#Running the logistic regression
			mylogit <- glm(phenoFactor ~ exp + ., data=confs, family="binomial")

		#Getting the results
			sumx = summary(mylogit)
		
			beta = sumx$coefficients["exp","Estimate"]
                	pvalue = sumx$coefficients['exp','Pr(>|z|)']
			se = sumx$coefficients["exp","Std. Error"]
                
                	cis = confint(mylogit, "exp", level=0.95)
                	lower = cis["2.5 %"]
                	upper = cis["97.5 %"]

		OUT[which(colnames(Phenodata[,-1])==i),1:12]<-c(workName, resType, beta, se, pvalue, data, beta, lower, upper, se, pvalue, n)
		print(OUT)
		}

	names(OUT)<-c("workName", "resType", "beta", "se", "pvalue", "data",Beta, Lower, Upper, Se, Pvalue, N)
	return(OUT) 
				
}


#-----------------------------------------------------------------------------------------

#For All

	O<-testBinary(dataset="All", Beta="beta_A", Pvalue="pvalue_A", Se="se_A", N="n_A", Lower="lower_A", Upper="upper_A", Phenodata=DP[,c(1,7:18)], Traitdata=TA, Covariatedata=C)
	FD_a<-O[,c(1,2,3:6)]
	RD_a<-O[,c(1,2,7:12)]
	FD_a
	RD_a

	write.table(FD_a,"./IntermediateData/DerivedBinaryForest_A.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_a,"./IntermediateData/DerivedBinaryResults_A.txt",sep="\t",quote=F,row.names=F) 


#For Ever
#Note: have to exclude initiation(9) 
	O<-testBinary(dataset="Ever", Beta="beta_E", Pvalue="pvalue_E", Se="se_E", N="n_E", Lower="lower_E", Upper="upper_E", Phenodata=DP[,c(1,7:8,10:18)], Traitdata=TE, Covariatedata=C)
	FD_e<-O[,c(1,2,3:6)]
	RD_e<-O[,c(1,2,7:12)]
	FD_e
	RD_e

	write.table(FD_e,"./IntermediateData/DerivedBinaryForest_E.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_e,"./IntermediateData/DerivedBinaryResults_E.txt",sep="\t",quote=F,row.names=F) 


#For Never
#Note: have to exclude: cessation (8), initiation(9), occasionalvsdaily (16)
names(DP)

	O<-testBinary(dataset="Never", Beta="beta_N", Pvalue="pvalue_N", Se="se_N", N="n_N", Lower="lower_N", Upper="upper_N", Phenodata=DP[,c(1,7,10:15,17,18)], Traitdata=TN, Covariatedata=C)
	FD_n<-O[,c(1,2,3:6)]
	RD_n<-O[,c(1,2,7:12)]
	FD_n
	RD_n

	write.table(FD_n,"./IntermediateData/DerivedBinaryForest_N.txt",sep="\t",quote=F,row.names=F) 
	write.table(RD_n,"./IntermediateData/DerivedBinaryResults_N.txt",sep="\t",quote=F,row.names=F) 

#----------------------------------------

#Then we a) concatenate the three FD (Forestplot data) datasets by workName 
#And b) merge the three RD (Results table data) datasets 

	dim(FD_a); dim(FD_e); dim(FD_n)
	dim(RD_a); dim(RD_e); dim(RD_n)

#Forest:
	Forest<-rbind(FD_a,FD_e,FD_n)
	F<-Forest[order(Forest$workName),]
	dim(F)

#Results:
	names(RD_a)
	#We join the results (we delete resType from the ever and never versions not to have that multiple times)
		Results<-join(RD_a, RD_e[,-2], by="workName", type="left")
		Results<-join(Results, RD_n[,-2], by="workName", type="left")
		dim(Results)

#We save the combined datasets
	write.table(F,"./IntermediateData/DerivedBinaryForest.txt",sep="\t",quote=FALSE,row.names=FALSE) 
	write.table(Results,"./IntermediateData/DerivedBinaryResults.txt",sep="\t",quote=FALSE,row.names=FALSE) 


#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################


#Combine with old binary results to create final forest and result tables

--------------------# Getting the ShortName and creating the variable Phenotype-------------------------

	ShortNames<-read.table(paste(WorkDir,"/FDR_initial/IntermediateData/ShortNames61Variables.txt",sep=""),sep="\t",quote="\"",header=TRUE)

#---------------------------
#FOREST
#---------------------------

#Reading in dervied binary
#	Fdb<-read.table(paste(WorkDir,"/FDR_final/IntermediateData/DerivedBinaryForest.txt",sep=""), sep="\t",header=TRUE)
	Fdb<-read.table("./IntermediateData/DerivedBinaryForest.txt", sep="\t",header=TRUE)
	dim(Fdb) #32 x 6	

		#> names(Fdb)
		#[1] "workName" "resType"  "beta"     "se"       "pvalue"   "data"

	#We create varName
		Fdb$varNamelong<-substring(Fdb$workName,2) #starting from the second character we copy workName to varNamelong, i.e we drop the d
		Fdb<-separate(Fdb, varNamelong, into = c("varName", "ending"), sep = "_", extra="merge") #So I can get Cat3 and ShortName from another file
		Fdb$newvarName<-paste("d",Fdb$varName,sep="") 

	#We create the variable NotFDRsig
		#checking the FDR significance (thresholds from: ../FDR_initial/Scripts/B_FDRthreshold.R)
			threshold_ALL <- 0.05*47/21094  	#0.0001114061
			threshold_EVER <- 0.05*29/16648		#8.709755e-05
			threshold_NEVER <- 0.05*2/16103		#6.210023e-06

		F<-Fdb
		F[which(F$data=="All"),c("NotFDRsig")] <- ifelse(F$pvalue[which(F$data=="All")] >= threshold_ALL,1,0)
		F[which(F$data=="Ever"),c("NotFDRsig")] <- ifelse(F$pvalue[which(F$data=="Ever")] >= threshold_EVER,1,0)
		F[which(F$data=="Never"),c("NotFDRsig")] <- ifelse(F$pvalue[which(F$data=="Never")] >= threshold_NEVER,1,0)
	
	#We get the Cat3
		F<-join(F, ShortNames[,c("varName","Cat3_Title")], by="varName", type="left")

	#We finalise the "ShortName"
		F$ending <- recode(F$ending, "TeaDrinker" = "Tea drinker", "CoffeeDrinker_excldecaf" = "Coffee drinker (excluding decaf)", "CoffeeDrinker_incldecaf" = "Coffee drinker (including decaf)","DecafvsCaf" = "Decaf vs caffeinated coffee",
			"DecafvsCaf" = "Decaf vs caffeinated coffee", "Decafvsnone" = "Decaf vs no coffee", "Groundvsnone" = "Ground vs no coffee", "Instvsnone" = "Instant vs no coffee", "InstvsOtherCaf"="Instant vs other caffeinated coffee", 
			"Othervsnone"="Other caffeinated coffee vs none", "OccasionalvsDaily"= "Occasional vs daily smoker")


	#We create the column Phenotype
		F$Phenotype <- paste(F$newvarName,F$ending,sep=" - ")

	#We only keep the wanted columns
		New <- F[,c("Phenotype", "beta","se","NotFDRsig","data", "Cat3_Title","resType","pvalue","varName")]	

#Reading in the old binary variables
	Fob = read.csv(paste(WorkDir,"/FDR_initial/IntermediateData/PHESANT_InitialResults_forForestquote_stacked.csv",sep=""), as.is=T, header=T)
	str(Fob)
	dim(Fob) #170 x 12

	#Taking the binary ones: Old binary
		table(Fob$resType) #13 in logistic binary
		Fob<-subset(Fob, Fob$resType == "LOGISTIC-BINARY")
		dim(Fob) #34 x 12

	#Deleting the ones I don't want anymore i.e. making sure we don't keep Smoking status
		Fob[,c("description")] #We see it is not included(had been analysed by PHESANT using "MULTINOMIAL-LOGISTIC")

	#We get the se
		Fob$se <- (Fob$beta-Fob$lower)/1.96

	#We add the variable NotFDRsig 
		Fob$NotFDRsig <- ifelse(Fob$fdrp>=0.05,1,0)

	#Only keeping wanted columns
		Fob<-Fob[,c("varName","resType","beta","se","NotFDRsig","pvalue","data", "Cat3_Title")]
	
	
	#changing coding in "data": ALL-->All and EVER-->Ever and NEVER-->Never
		Fob$data<-recode(Fob$data, "ALL" = "All", "EVER" = "Ever", "NEVER" = "Never")

	#Adding variable Phenotype
		F<-join(Fob, ShortNames[,c("varName","ShortName")], by="varName", type="left")
		F$Phenotype <- paste(F$varName,F$ShortName,sep=" - ")

	#We only keep the wanted columns
		Old <- F[,c("Phenotype", "beta","se","NotFDRsig","data", "Cat3_Title","resType","pvalue","varName")]	

	#Concatenating them 
		Wholeset<-rbind(Old,New)
	
#Ordering based on Cat3_Title and Pheno
	FOREST <- Wholeset[order(Wholeset$Cat3_Title,Wholeset$Pheno),]

length(unique(FOREST$Phenotype)) #25

#Table for creating the Forest plot
	write.table(FOREST,"./IntermediateData/Final_25_BinaryForest.txt",sep="\t",quote=TRUE,row.names=FALSE) 
	write.table(FOREST,"./IntermediateData/Final_25_BinaryForest.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)

#We create the following readme_Final_25_BinaryForest.txt to accompary it (can then copy paste this to another sheet inside the excel)
Phenotype: d = derived
beta: coefficient for the GRS from the regression model
se: standard error for beta
NotFDRsig: 1=Yes (i.e. not FDR significant) 0=No (ie. IS FDR significant) This will be used to fill in the circles in the plot command (fill if value below 0.05)
data: which dataset, All/Ever/Never
Cat3_Title: The UKBB category
---the below should not be needed for plotting-------
resType: regression model used
pvalue: pvalue for the beta
varName: Data Field in UKBB



#---------------------------
#RESULTS
#---------------------------	

#Reading in derived binary data
#	Rdb<-read.table(paste(WorkDir,"/FDR_final/IntermediateData/DerivedBinaryResults.txt",sep=""), sep="\t",header=TRUE)
	Rdb<-read.table("./IntermediateData/DerivedBinaryResults.txt", sep="\t",header=TRUE)
	dim(Rdb) #12 x20

	#We create varName
		Rdb$varNamelong<-substring(Rdb$workName,2) #starting from the second character we copy workName to varNamelong, i.e we drop the d
		Rdb<-separate(Rdb, varNamelong, into = c("varName", "ending"), sep = "_", extra="merge") #So I can get Cat3 and ShortName from another file
		Rdb$newvarName<-paste("d",Rdb$varName,sep="") 

	#We finalise the "ShortName"
		Rdb$ending <- recode(Rdb$ending, "TeaDrinker" = "Tea drinker", "CoffeeDrinker_excldecaf" = "Coffee drinker (excluding decaf)", "CoffeeDrinker_incldecaf" = "Coffee drinker (including decaf)","DecafvsCaf" = "Decaf vs caffeinated coffee",
			"DecafvsCaf" = "Decaf vs caffeinated coffee", "Decafvsnone" = "Decaf vs no coffee", "Groundvsnone" = "Ground vs no coffee", "Instvsnone" = "Instant vs no coffee", "InstvsOtherCaf"="Instant vs other caffeinated coffee", 
			"Othervsnone"="Other caffeinated coffee vs none", "OccasionalvsDaily"= "Occasional vs daily smoker")

	#Creating the variable Phenotype
		Rdb$Phenotype <- paste(Rdb$newvarName,Rdb$ending,sep=" - ")
		Rdb$ShortName<- Rdb$ending

	#Forgot to save the Cat3_Title variable so have to add that
		New<-join(Rdb, ShortNames[,c("varName","Cat3_Title","description")], by="varName", type="left")


#Reading in the old binary variables
		Rob = read.csv(paste(WorkDir,"/FDR_initial/IntermediateData/PHESANT_InitialResults_forForestquote.csv",sep=""), as.is=T, header=T)
		str(Rob)
		dim(Rob) #61 26

	#Taking the binary ones: Old binary
		table(Rob$resType) #13 in logistic binary
		Rob<-subset(Rob, Rob$resType == "LOGISTIC-BINARY")
		dim(Rob) #13 26

	#Deleting the ones I don't want anymore i.e. making sure we don't keep Smoking status
		Rob[,c("description")] #We see it is not included(had been analysed by PHESANT using "MULTINOMIAL-LOGISTIC")

	#Getting ShortName
		Results<-join(Rob, ShortNames[,c("varName","ShortName")], by="varName", type="left")

	#Adding Pheno in style varName-description
		Results$Phenotype<-paste(Results$varName,Results$ShortName,sep=" - ")
		Results$Phenotype


#Concatenating them 
	New<-New[,c("Phenotype", "resType", "Cat3_Title",
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", 
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", 
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", 
				"varName", "description","ShortName")]
	Old<-Results[,c("Phenotype", "resType", "Cat3_Title",
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", 
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", 
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", 
				"varName", "description","ShortName")]
	Wholeset<-rbind(Old,New)


#Adding the following missing columns: "FDRsig_A","FDRsig_E","FDRsig_N", "FDRsigSum","pvalue_EvsN", "EverNeversig"

	#We start with: "FDRsig_A","FDRsig_E","FDRsig_N", "FDRsigSum"
		#checking the FDR significance (thresholds from: ../FDR_initial/Scripts/B_FDRthreshold.R)
		threshold_ALL <- 0.05*47/21094  	#0.0001114061
		threshold_EVER <- 0.05*29/16648		#8.709755e-05
		threshold_NEVER <- 0.05*2/16103		#6.210023e-06

		Wholeset$FDRsig_A <- ifelse(Wholeset$pvalue_A < threshold_ALL,1,0)
		Wholeset$FDRsig_E <- ifelse(Wholeset$pvalue_E < threshold_EVER,1,0)
		Wholeset$FDRsig_N <- ifelse(Wholeset$pvalue_N < threshold_NEVER,1,0)
		Wholeset$FDRsigSum <- rowSums(Wholeset[,c("FDRsig_A", "FDRsig_E","FDRsig_N")], na.rm=TRUE)

#EvervsNever
	#We create the variables: "pvalue_EvsN", "EverNeversig"
	#Then testing if any have a stat. sig. difference when comparing the betas between Ever and Never
		Results<-Wholeset
		threshold_EvsN <- 0.05*2/15212
		Results$x = (Results$beta_E-Results$beta_N)^2 / (Results$se_E^2+Results$se_N^2)
      		Results$pvalue_EvsN = pchisq(Results$x,df=1,lower=FALSE)
		dim(Results) #25 x 30

		Results$EverNeversig <- ifelse(Results$pvalue_EvsN < threshold_EvsN,1,0)
		dim(subset(Results, Results$EverNeversig==1)) #0 x31

#Finally saving in the wanted order:

	#Ordering based on Cat3_Title and Pheno
		RESULTS <- Results[order(Results$Cat3_Title,Results$Phenotype),]

	#Get wanted columns in wanted order
		RESULTS <- RESULTS[,c("Phenotype", "resType", "Cat3_Title",
				"beta_A", "lower_A", "upper_A", "se_A", "pvalue_A", "n_A", "FDRsig_A",
				"beta_E", "lower_E", "upper_E", "se_E", "pvalue_E", "n_E", "FDRsig_E",
				"beta_N", "lower_N", "upper_N", "se_N", "pvalue_N", "n_N", "FDRsig_N",
				"varName", "description", "ShortName", "FDRsigSum","pvalue_EvsN", "EverNeversig")]


#We save the results file as a supplementary table
#We create the following readme_Final_25_BinaryResults.txt to accompary it (can then copy paste this to another sheet inside the excel)
#nano ./IntermediateData/readme_Final_25_BinaryResults.txt 
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
	#Results table that has to be concatenated with the ordinal and linear ones. 
		write.table(RESULTS,"./IntermediateData/Final_25_BinaryLogisticResults.txt",sep="\t",quote=TRUE,row.names=FALSE) 
		write.table(RESULTS,"./IntermediateData/Final_25_BinaryResults.csv",sep=",",quote=TRUE,row.names=FALSE, col.names=TRUE)




